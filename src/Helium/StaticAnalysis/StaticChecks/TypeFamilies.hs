{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helium.StaticAnalysis.StaticChecks.TypeFamilies where

import Helium.Syntax.UHA_Syntax
import Helium.StaticAnalysis.Messages.StaticErrors
import Helium.StaticAnalysis.Messages.Warnings
import Helium.StaticAnalysis.Messages.Messages
import Helium.StaticAnalysis.Miscellaneous.Unify
import Helium.StaticAnalysis.Inferencers.OutsideInX.TopConversion
    ( typeToMonoType, tpToMonoType, importEnvironmentToTypeFamilies, TypeFamilies, tfInstanceInfoToAxiom, tfInstanceInfoToMonoTypes, typeSynonymsToTypeFamilies )
import Debug.Trace
import Helium.StaticAnalysis.Inferencers.OutsideInX.Rhodium.RhodiumTypes
    ( isFamilyFree, MonoType (MonoType_Fam, MonoType_Var, MonoType_Con, MonoType_App), MonoTypes, Axiom (Axiom_Unify), TyVar, fvToList )
import Helium.StaticAnalysis.Miscellaneous.TypeConversion
    ( namesInType, namesInTypes )
import Data.List (nub, elemIndex, (\\))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S
import Helium.Utils.Utils (internalError)
import Unbound.Generics.LocallyNameless (runFreshM)
import Top.Types (unquantify, split)
import Helium.ModuleSystem.ImportEnvironment ( TypeSynonymEnvironment )
import Helium.StaticAnalysis.StaticChecks.TypeFamilyInfos
import Helium.StaticAnalysis.Miscellaneous.ConstraintInfoOU
import Unbound.Generics.LocallyNameless.Operations (unbind)
import Unbound.Generics.LocallyNameless.Fresh (FreshM)

-- Checks if a MonoType is as a whole a type family application
isTFApplication :: MonoType -> Bool 
isTFApplication (MonoType_Fam _ _) = True 
isTFApplication _                  = False

-- Checks if the MonoType is a bare variable
isBareVariable :: MonoType -> Bool
isBareVariable (MonoType_Var _ _) = True
isBareVariable _                  = False

-- Builds Injective environment.
buildInjectiveEnv :: TFDeclInfos -> InjectiveEnv
buildInjectiveEnv
  = foldr (\ d
          -> M.insert
                (show $ tfdName d) (getInjIndices (argNames d) (injNames d))
          )
          M.empty

getInjIndices :: Names -> Maybe Names -> [Int]
getInjIndices _  Nothing = []
getInjIndices ns (Just ins) = map (fromJust . flip elemIndex ins) ns

--------------------------------------
-- Declaration static checks, SEPARATE CHECKS

-- Checks if a declaration does not have variables with identical names.
declNoIndenticalVars :: Declaration -> Errors
declNoIndenticalVars (Declaration_TypeFam _ (SimpleType_SimpleType _ _ tv) _ _) 
  = [Duplicated Variable [n1, n2] | (n1, n2) <- createPairs tv, n1 == n2]
declNoIndenticalVars _ = []

-- Check the injectivity variables (both result var and injectively defined vars)
declInjectivityVars :: Declaration -> Errors
declInjectivityVars (Declaration_TypeFam _ _ MaybeInjectivity_Nothing  _)   = []
declInjectivityVars (Declaration_TypeFam _ (SimpleType_SimpleType _ _ tv) (MaybeInjectivity_Just inj) _) = let
  (Injectivity_Injectivity declres res injargs) = inj

  in [Undefined Variable res [declres] ["The result variable in the injectivity annotation is not defined!"] | res /= declres] ++
     [Undefined Variable x tv [] | x <- injargs, x `notElem` tv]
declInjectivityVars _ = []

------------------------------------------------------------------
-- STATIC CHECKS OVER ALL KNOWN DECLARATIONS AND INSTANCES
checkTypeFamStaticErrors :: TypeSynonymEnvironment -> TFDeclInfos -> TFInstanceInfos -> Errors
checkTypeFamStaticErrors env dis iis = let
  
  tyfams = typeSynonymsToTypeFamilies env ++ obtainTyFams dis

  -- The simple duplication check and ast var alignment.
  phase1 = declCheckDuplicates dis

  -- InstanceValidity is a big one, consists of multiple possible errors
  -- New phase because duplicates generate errors in this area too.
  phase2 = atsCheckVarAlignment dis iis
           ++ instCheckInstanceValidity dis iis
           ++ instSaturationCheck dis iis

  -- Phase three is initiated when instances are syntactically valid
  -- These checks are based on theory rather than syntax.
  phase3 = instNoTFInArgument tyfams iis
           ++ instVarOccursCheck iis
           ++ instInjDefChecks tyfams dis iis
           ++ instSmallerChecks tyfams dis iis
           ++ compatibilityCheck tyfams iis
           ++ injChecks tyfams dis iis
  in phase1 >>\ phase2 >>\ phase3

checkTypeFamWarnings :: TypeSynonymEnvironment -> TFDeclInfos -> TFInstanceInfos -> Warnings
checkTypeFamWarnings env dis tis = let
  tyfams = typeSynonymsToTypeFamilies env ++ obtainTyFams dis

  in closedOverlapWarning tyfams tis

-- Check whether duplicate type families exist.
declCheckDuplicates :: TFDeclInfos -> Errors
declCheckDuplicates tfs = let

  -- Creates all unique pairs of declaration names
  createNamePairs xs = [(n1, n2) | (n1:ys) <- tails (map tfdName xs), n2 <- ys]

  in [DuplicateTypeFamily (n1, n2) | (n1, n2) <- createNamePairs tfs, n1 == n2]

--------------------------------------
-- Associated Typesynonym check (variable alignment)
atsCheckVarAlignment :: TFDeclInfos -> TFInstanceInfos -> Errors
atsCheckVarAlignment decls insts = let

  -- Converts all instances to a tuple containing (class instance name, tf instance name, monotypes of instance args, monotypes of tf instance args)
  convertedInsts = [(fromJust (classIName inst), tfiName inst, map (typeToMonoType []) (fromJust $ classTypes inst), map (typeToMonoType []) (argTypes inst)) 
                   | inst <- insts
                   , tfiType inst == ATS]

  violations = [(itfn, instn, thrd (its !! ci), thrd (tfts !! tfi)) |
                decl <- decls, -- Get a decl
                tfdType decl == ATS,
                (ci, tfi) <- fromJust $ classIdx decl, -- obtain alignment info
                (instn, itfn,  its, tfts) <- convertedInsts, -- Get an inst
                tfdName decl == itfn, -- instance and decl names must be equal 
                thrd (its !! ci) /= thrd (tfts !! tfi)] -- Then types at indices must be equal.

  in [WronglyAlignedATS n1 n2 t1 t2 | (n1, n2, t1, t2) <- violations]

--------------------------------------
-- Instance checks

instCheckInstanceValidity :: TFDeclInfos -> TFInstanceInfos -> Errors
instCheckInstanceValidity ds is = let

  ns = map tfdName ds
  
  -- Obtains open and closed instances
  obtainOpenClosed :: TFInstanceInfos -> Names
  obtainOpenClosed = map tfiName . filter (\ti -> tfiType ti == Open || tfiType ti == Closed)

  --Obtains assoc and open instances
  obtainOpenAssoc :: TFInstanceInfos -> Names
  obtainOpenAssoc = map tfiName . filter (\ti -> tfiType ti == Open || tfiType ti == ATS)

  hasAssocTypeSyn :: Maybe Name -> Name -> Bool
  hasAssocTypeSyn cn n = case filter (\d -> tfdType d == ATS 
                                      && classDName d == cn 
                                      && tfdName d == n) ds
                          of
                            [] -> False
                            _  -> True

  -- Obtains instance names of undefined type families
  getUndefinedNames = [n1 | n1 <- map tfiName is, n1 `notElem` ns]

  -- Associated type synonym instance validities
  assocNotInClassInstance = [(n2, fromJust $ classDName d) | d <- ds, n2 <- obtainOpenClosed is, tfdName d == n2, tfdType d == ATS]
  assocInstanceNotPartOfClass = [(tfiName i, fromJust $ classIName i) | i <- is, not $ hasAssocTypeSyn (classIName i) (tfiName i)
                                , tfiName i `notElem` getUndefinedNames, tfiType i == ATS]
  assocNotLinkedToRightClass = [(tfiName i, fromJust $ classIName i, fromJust $ classDName d) | d <- ds, i <- is
                               , tfdName d == tfiName i, classDName d /= classIName i
                               , tfdType d == ATS, tfiType i == ATS]

  -- Closed type synonym instance validities.
  openInstancesForClosed = [n2 | d <- ds, n2 <- obtainOpenAssoc is, tfdName d == n2, tfdType d == Closed]

  in [UndefinedTypeFamily n ns | n <- getUndefinedNames] ++
     [ATSNotInInstance n cn | (n, cn) <- assocNotInClassInstance] ++
     [ATSWrongClassInstance n cn1 cn2 | (n, cn1, cn2) <- assocNotLinkedToRightClass] ++
     [ATSNotPartOfClass n1 n2 | (n1, n2) <- assocInstanceNotPartOfClass] ++
     [OpenInstanceForClosed n | n <- openInstancesForClosed]

-- Checks whether the type fam instance LHS is fully saturated.
instSaturationCheck :: TFDeclInfos -> TFInstanceInfos -> Errors
instSaturationCheck ds is = let

  getNameArgs :: TFDeclInfo -> (Name, Names)
  getNameArgs d = (tfdName d, argNames d) 

  getNameTypes :: TFInstanceInfo -> (Name, Types)
  getNameTypes i = (tfiName i, argTypes i)

  violations = [(n2, length ns, length ts) | (n1, ns) <- map getNameArgs ds, (n2, ts) <- map getNameTypes is, n1 == n2, length ns /= length ts]

  in [WronglySaturatedTypeFamily n dl tl | (n, dl, tl) <- violations]

-- Checks if all arguments are type family free.
instNoTFInArgument :: TypeFamilies -> TFInstanceInfos -> Errors
instNoTFInArgument fams tis = let

  violations = [ (tfiName inst, arg, thrd $ typeToMonoType fams arg) |
                 inst <- tis
               , arg <- argTypes inst
               , not $ isFamilyFree $ thrd $ typeToMonoType fams arg
               ]
  in [TFInArgument n t mt | (n, t, mt) <- violations] 

-- Checks if all vars in RHS are in scope wrt the LHS.
instVarOccursCheck :: TFInstanceInfos -> Errors
instVarOccursCheck tis = let

  getViolations :: TFInstanceInfo -> Names
  getViolations tfi = let
    argVars = namesInTypes $ argTypes tfi
    defVars = namesInType $ defType tfi

    undefNames = [n | n <- defVars, n `notElem` argVars]
    in undefNames
    
  in [Undefined Variable n [] ["Variable " ++ show (show n) ++ " does not occur in any instance argument"] | n <- concatMap getViolations tis]

-- Checks whether non-injective type families are always becoming 'smaller' (termination guarantees)
instSmallerChecks :: TypeFamilies -> TFDeclInfos -> TFInstanceInfos -> Errors
instSmallerChecks tyFams dis tis = let

  obtainDefTyFam :: MonoType -> MonoTypes
  obtainDefTyFam t@(MonoType_Fam _ mts) = t : concatMap obtainDefTyFam mts
  obtainDefTyFam (MonoType_App mt1 mt2) = obtainDefTyFam mt1 ++ obtainDefTyFam mt2
  obtainDefTyFam _                      = []
  
  obtainNameArgsDef :: TFInstanceInfo -> (Name, Types, Type)
  obtainNameArgsDef i = (tfiName i, argTypes i, defType i)

  obtainVars :: MonoType -> [String]
  obtainVars (MonoType_Var (Just s) _) = [s]
  obtainVars (MonoType_Con _)          = []
  obtainVars (MonoType_Fam _ mts)      = nub $ concatMap obtainVars mts
  obtainVars (MonoType_App mt1 mt2)    = nub $ obtainVars mt1 ++ obtainVars mt2

  countSymbols :: MonoType -> Int
  countSymbols (MonoType_Var _ _)     = 1
  countSymbols (MonoType_Con _)       = 1
  countSymbols (MonoType_Fam _ mts)   = sum $ map countSymbols mts
  countSymbols (MonoType_App mt1 mt2) = countSymbols mt1 + countSymbols mt2

  countOccVar :: String -> MonoType -> Int
  countOccVar v (MonoType_Var (Just s) _) | v == s = 1
                                          | otherwise = 0
  countOccVar v (MonoType_Fam _ mts)      = sum $ map (countOccVar v) mts
  countOccVar v (MonoType_App mt1 mt2)    = countOccVar v mt1 + countOccVar v mt2
  countOccVar _ _                         = 0

  checkInstance :: (Name, Types, Type) -> Errors
  checkInstance (n, ts, t) = let
    argMts = map (thrd . typeToMonoType tyFams) ts
    defMt = thrd $ typeToMonoType tyFams t 

    defTFs = obtainDefTyFam defMt
    -- First smaller check (def must be family free)
    notTFFree = case filter (\(MonoType_Fam _ mts) -> not $ all isFamilyFree mts) defTFs of
                  [] -> []
                  xs -> [TFFamInDefNotFamFree n xs]

    -- Second smaller check 
    symbolsNotSmaller = [ TFDefNotSmallerSymbols n tf | tf <- defTFs, sum (map countSymbols argMts) <= countSymbols tf ]

    -- Third smaller check
    varOccurenceCheck = [ TFDefVarCountNotSmaller n argVar | 
                          argVar <- concatMap obtainVars argMts
                        , defTF <- defTFs
                        , sum (map (countOccVar argVar) argMts) <= countOccVar argVar defTF]
    in notTFFree ++ symbolsNotSmaller ++ varOccurenceCheck
  -- checks are done over non-injective families.
  in concatMap (checkInstance . obtainNameArgsDef) (tis \\ obtainInjectiveTFInstances dis tis)

-- Definition of instance may not be a direct type family, and,
-- if a definition is a bare var, every argument must be too!
instInjDefChecks :: TypeFamilies -> TFDeclInfos -> TFInstanceInfos -> Errors
instInjDefChecks tyFams dis tis = let

  isInjective :: TFDeclInfo -> TFInstanceInfo -> Bool
  isInjective d i = injective d && tfdName d == tfiName i

  injInsts = [inst | inst <- tis, decl <- dis, isInjective decl inst]
  tyFamDef = [ tfiName inst | 
               inst <- injInsts
             , isTFApplication (thrd $ typeToMonoType tyFams (defType inst))]
  bareVars = [ (tfiName inst, filter (not . isBareVariable) (map (thrd . typeToMonoType tyFams) (argTypes inst))) |
               inst <- injInsts
             , isBareVariable (thrd $ typeToMonoType tyFams (defType inst))
             , not $ all (isBareVariable . thrd . typeToMonoType tyFams) (argTypes inst)]
  in [InjTFInDefinition n | n <- tyFamDef] ++
     [InjBareVarInDefinition n ns | (n, ns) <- bareVars]

-------------------------------
-- COMPATIBILITY CHECK (checks whether open type families overlap)
compatibilityCheck :: TypeFamilies -> TFInstanceInfos -> Errors
compatibilityCheck tfams tis = let
  
  -- Obtain all open tf instances
  openTFs = filter (\i -> tfiType i == Open) tis
  
  -- create to be checked pairs
  instancePairs = [(inst1, inst2) | 
                  inst1:ys <- tails openTFs, inst2 <- ys
                  , tfiName inst1 == tfiName inst2]

  -- In compat
  in mapMaybe (compat tfams) instancePairs

-- Entrypoint for closed overlap warning.
closedOverlapWarning :: TypeFamilies -> TFInstanceInfos -> Warnings
closedOverlapWarning tfams tis = let

  closedTFs = filter (\i -> tfiType i == Closed) tis
  instancePairs = [(inst1, inst2) | 
                  inst1 <- closedTFs, inst2 <- closedTFs
                  , tfiName inst1 == tfiName inst2
                  , priority inst1 < priority inst2 ]

  in mapMaybe (compatWarn tfams) instancePairs

-- compatibility check of two instances (pairwise).
compat :: TypeFamilies -> (TFInstanceInfo, TFInstanceInfo) -> Maybe Error
compat tfams (inst1, inst2) = let

  axiom1 = tfInstanceInfoToAxiom tfams inst1
  axiom2 = tfInstanceInfoToAxiom tfams inst2

  ((lhs1, rhs1), (lhs2, rhs2)) = runFreshM $ unbindAx axiom1 axiom2

  in case unifyTy lhs1 lhs2 of
            SurelyApart -> Nothing
            Unifiable subst -> 
              if applySubst subst rhs1 == applySubst subst rhs2
                then Nothing
                else Just $ OpenTFOverlapping (tfiName inst1) (tfiName inst2) lhs1 lhs2 rhs1 rhs2

-- warning version of compat, uses matchTy to check unification only in one way.
compatWarn :: TypeFamilies -> (TFInstanceInfo, TFInstanceInfo) -> Maybe Warning
compatWarn tfams (inst1, inst2) = let
  
  axiom1 = tfInstanceInfoToAxiom tfams inst1
  axiom2 = tfInstanceInfoToAxiom tfams inst2

  ((lhs1, _), (lhs2, rhs2)) = runFreshM $ unbindAx axiom1 axiom2
  in case matchTy lhs1 lhs2 of
            SurelyApart -> Nothing
            Unifiable _ -> Just $ OverlappedClosedTypeFamilyInstance (tfiName inst1) lhs2 rhs2

-- Performs pairwise injectivity check
-- Performs whether type vars in injective arguments are used injectively in the definition.
injChecks :: TypeFamilies -> TFDeclInfos -> TFInstanceInfos -> Errors
injChecks fams dis tis = let

  injTFs = obtainInjectiveTFInstances dis tis
  instancePairs = [(inst1, inst2) 
                  | inst1:ys <- tails injTFs, inst2 <- ys
                  , tfiName inst1 == tfiName inst2]
                  ++ [(inst, inst) | inst <- injTFs] -- Adding pairwise checks with itself (NEEDED)!
  ienv = buildInjectiveEnv dis
  in mapMaybe (pairwiseInjCheck fams ienv) instancePairs
  ++ mapMaybe (wronglyUsedVarInInjCheck fams ienv) injTFs

pairwiseInjCheck :: TypeFamilies -- Type fams to build axioms
                 -> InjectiveEnv -- Map containing indices of injective type families.
                 -> (TFInstanceInfo, TFInstanceInfo) -- The two instances to be pairwise checked
                 -> Maybe Error -- Result is a maybe error
pairwiseInjCheck fams ienv (inst1, inst2) = let

  axiom1 = tfInstanceInfoToAxiom fams inst1
  axiom2 = tfInstanceInfoToAxiom fams inst2
  
  ((clhs1@(MonoType_Fam _ lhs1), rhs1), (clhs2@(MonoType_Fam _ lhs2), rhs2)) = runFreshM $ unbindAx axiom1 axiom2

  in case preUnify ienv rhs1 rhs2 of
            -- RHSs surely apart, No error, no clashing.
            SurelyApart -> Nothing
            -- RHSs may unify (under preunification).
            Unifiable subst
              -- If all injective arguments agree with each other, there is no problem.
              | all (\i -> checkInjArg subst (lhs1 !! i) (lhs2 !! i)) injIndices
                  -> Nothing 
              -- In case of a closed type family, we are good when the lhs's are not equal after applying the substitution.
              | tfiType inst1 == Closed && applySubst subst clhs1 /= applySubst subst clhs2
                  -> Nothing
              -- Otherwise, the two instances violate the injectivity property.
              | otherwise
                  -> Just $ InjPreUnifyFailed (tfiName inst1) (tfiName inst2) clhs1 clhs2 rhs1 rhs2  -- ERROR!
  where
    injIndices = ienv M.! show (tfiName inst1)

    checkInjArg :: SubstitutionEnv -> MonoType -> MonoType -> Bool
    checkInjArg subst mt1 mt2 = applySubst subst mt1 == applySubst subst mt2

-- Running this in the FreshM monad makes sure that the unbind functions generate fresh vars in the lhs and rhs's.
unbindAx :: Axiom ConstraintInfo -> Axiom ConstraintInfo -> FreshM ((MonoType, MonoType), (MonoType, MonoType))
unbindAx (Axiom_Unify b1) (Axiom_Unify b2) = do
  (_, (lhs1, rhs1)) <- unbind b1
  (_, (lhs2, rhs2)) <- unbind b2
  return ((lhs1, rhs1), (lhs2, rhs2))

-- Performs whether type vars in injective arguments are used injectively in the definition.
wronglyUsedVarInInjCheck :: TypeFamilies -> InjectiveEnv -> TFInstanceInfo -> Maybe Error
wronglyUsedVarInInjCheck fams ienv inst = let

  (Axiom_Unify b) = tfInstanceInfoToAxiom fams inst
  (_, (lhs@(MonoType_Fam f mts), rhs)) = runFreshM $ unbind b
  
  injMts = map (mts !!) $ ienv M.! f
  -- gets the vars in the injective arguments of the LHS
  lhsVars = foldl (\s mt -> s `S.union` varTupleSet mt) S.empty injMts
  -- gets the injectively used vars in the RHS
  rhsVars = injVarsOfMonoType ienv False rhs
  -- Every var in the injective arguments must be used injectively in the RHS
  badVars = lhsVars S.\\ rhsVars  

  in if S.null badVars
      then Nothing
      else  Just $ InjWronglyUsedVars (tfiName inst) lhs rhs (map fst $ S.toList badVars)

-- TyVar forgets the name of the variable and we therefore return a tuple containing the name
-- in the form of a string.

-- Simply obtains the vars in a monotype.
varTupleSet :: MonoType -> S.Set (String, TyVar)
varTupleSet (MonoType_Var (Just s) v) = S.singleton (s, v)
varTupleSet (MonoType_Con _)          = S.empty 
varTupleSet (MonoType_App mt1 mt2)    = varTupleSet mt1 `S.union` varTupleSet mt2
varTupleSet (MonoType_Fam _ mts)      = foldl (\s mt -> s `S.union` varTupleSet mt) S.empty mts

-- Obtains injective vars inside the RHS
injVarsOfMonoType :: InjectiveEnv -> Bool -> MonoType -> S.Set (String, TyVar)
injVarsOfMonoType _   _          (MonoType_Var (Just s) v) = S.singleton (s, v)
injVarsOfMonoType _   _          (MonoType_Con _)   = S.empty 
injVarsOfMonoType env look_under (MonoType_App mt1 mt2) 
  = injVarsOfMonoType env look_under mt1 `S.union` injVarsOfMonoType env look_under mt2
injVarsOfMonoType env look_under (MonoType_Fam f mts)
  = case M.lookup f env of
    Nothing -> S.empty
    Just idxs
      | look_under ->  let
        injArgs = map (mts !!) idxs
        in foldl (\s mt -> s `S.union` injVarsOfMonoType env look_under mt) S.empty injArgs
      | otherwise -> S.empty
-- CHECKS TODO!!!!!:
-- unused var check for injective type families.
