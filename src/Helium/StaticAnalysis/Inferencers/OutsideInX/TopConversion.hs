{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Helium.StaticAnalysis.Inferencers.OutsideInX.TopConversion(
        monoTypeToTp
    ,   tpSchemeListDifference
    ,   bindVariables
    ,   typeToPolytype
    ,   typeToMonoType
    ,   getTypeVariablesFromMonoType
    ,   tpSchemeToMonoType
    ,   tpSchemeToPolyType
    ,   tpSchemeToPolyType'
    ,   polyTypeToTypeScheme
    ,   classEnvironmentToAxioms
    ,   typeSynonymsToAxioms
    ,   getTypeVariablesFromPolyType
    ,   getTypeVariablesFromPolyType'
    ,   getTypeVariablesFromConstraints
    ,   getConstraintFromPoly
    ,   polytypeToMonoType
    ,   unbindPolyType
    ,   importEnvironmentToTypeFamilies

) where

import Unbound.LocallyNameless hiding (Name, freshen)
import Unbound.LocallyNameless.Types (GenBind(..))
import Top.Types.Classes
import Top.Types.Primitive
import Top.Types.Quantification
import Top.Types.Qualification
import Top.Types.Substitution
import Top.Types.Schemes
import Helium.Syntax.UHA_Syntax
import Helium.Syntax.UHA_Utils
import Helium.Utils.Utils
import Helium.StaticAnalysis.Miscellaneous.TypeConversion
import Helium.StaticAnalysis.Miscellaneous.ConstraintInfoOU
import Helium.StaticAnalysis.Inferencers.OutsideInX.Rhodium.RhodiumTypes
import Helium.ModuleSystem.ImportEnvironment
import qualified Data.Map as M
import Control.Monad.State
import Control.Arrow
import Data.Maybe 
import Data.List
import Debug.Trace
import Data.Functor.Identity

import Unbound.LocallyNameless.Fresh
import Unbound.LocallyNameless.Ops hiding (freshen)
import Unbound.LocallyNameless.Types hiding (Name)

deriving instance Show Type
deriving instance Show ContextItem 

type TypeFamilies = [(String, Int)]

bindVariables :: [TyVar] -> PolyType -> PolyType
bindVariables = flip (foldr ((PolyType_Bind "0" .) . bind))

bindVariables' :: [(String, TyVar)] -> PolyType -> PolyType
bindVariables' = flip (foldr (\(s, t) p -> PolyType_Bind s (bind t p)))


monoTypeToTp :: MonoType -> Tp
monoTypeToTp (MonoType_App (MonoType_Con "[]") (MonoType_Con "Char")) = TCon "String"
monoTypeToTp (MonoType_Var _ n) = TVar (fromInteger (name2Integer n))
monoTypeToTp (MonoType_Con n)   = TCon n
monoTypeToTp (MonoType_App f a) = TApp (monoTypeToTp f) (monoTypeToTp a)
monoTypeToTp (MonoType_Fam s a) = foldl TApp (TCon s) (map monoTypeToTp a)

polyTypeToTypeScheme :: PolyType -> TpScheme
polyTypeToTypeScheme p = let
        (quant, preds, tp) = runFreshM (ptHelper p)
        qualifiedType = preds .=>. tp
        in bindTypeVariables quant qualifiedType
    where
        constraintToPredicate :: Constraint -> [Predicate]
        constraintToPredicate (Constraint_Class c mts _) = map (\m -> Predicate c $ monoTypeToTp m) mts
        ptHelper :: PolyType -> FreshM ([Int], [Predicate], Tp)
        ptHelper (PolyType_Bind s b) = do
            (t, p) <- unbind b
            (qs, ps, tp) <- ptHelper p
            return (fromInteger (name2Integer t) : qs, ps, tp)
        ptHelper (PolyType_Mono cs m) = do
            return ([], concatMap constraintToPredicate cs, monoTypeToTp m)

importEnvironmentToTypeFamilies :: ImportEnvironment -> TypeFamilies
importEnvironmentToTypeFamilies = map (\(n, (i, _)) -> (show n, i)) . M.assocs . typeSynonyms


tpSchemeListDifference :: M.Map Name TpScheme -> M.Map Name TpScheme -> M.Map Name  ((Tp, String), (Tp, String))
tpSchemeListDifference m1 m2 = M.map fromJust $ M.filter isJust $ M.intersectionWith eqTpScheme m1 m2

eqTpScheme :: TpScheme -> TpScheme -> Maybe ((Tp, String), (Tp, String))
eqTpScheme t1@(Quantification (is1, qmap1, tp1)) t2@(Quantification (is2, qmap2, tp2)) = let
    subs = M.fromList $ zipWith (\orig rep -> (orig, TVar rep)) is2 is1
    tp2r = subs |-> unqualify tp2
    tp1r = unqualify tp1
    in if freshen (0 :: Int) tp1r == freshen 0 tp2r  then Nothing else Just ((tp1r, "Orig"), (tp2r, "OutsideIn(X)"))

typeToPolytype :: TypeFamilies -> Integer -> Type -> (PolyType, Integer, [(String, TyVar)])
typeToPolytype fams bu t = let
    (cs, tv, mt) = typeToMonoType fams t
    (mapping, (mt', bu')) = freshenWithMapping [] bu mt
    mappingSub :: [(TyVar, MonoType)]
    mappingSub = map (\(i, v) -> (integer2Name i, var (integer2Name v))) mapping
    cs' = map (substs mappingSub) cs 
    qmap = getQuantorMap (makeTpSchemeFromType t)
    mapping' :: [(String, TyVar)]
    mapping' = map (\(o, s) -> (fromMaybe (internalError "TopConversion.hs" "typeToPolytype" "Type variable not found") $ lookup (fromInteger o) qmap, integer2Name s)) mapping
    vars = getTypeVariablesFromMonoType mt'
    in (foldr (\(s, b) p -> PolyType_Bind s (bind b p)) (PolyType_Mono cs' mt') mapping', bu', mapping')

typeToMonoType :: TypeFamilies -> Type -> ([Constraint ], [(String, TyVar)], MonoType)
typeToMonoType fams = tpSchemeToMonoType fams . makeTpSchemeFromType

tpSchemeToPolyType :: TypeFamilies -> TpScheme -> PolyType
tpSchemeToPolyType fams = fst . tpSchemeToPolyType' fams []

tpSchemeToPolyType' :: TypeFamilies -> [String] -> TpScheme -> (PolyType, [(String, TyVar)])
tpSchemeToPolyType' fams restricted tps = let 
        (cs, tv, mt) = tpSchemeToMonoType fams tps
        pt' = PolyType_Mono cs mt
        pt = bindVariables' tv pt'
        --pt = bindVariables (map snd tv) pt'
    in (pt, tv) 

tpSchemeToMonoType :: TypeFamilies -> TpScheme -> ([Constraint], [(String, TyVar)], MonoType)
tpSchemeToMonoType fams tps = 
    let 
        qmap = map (\(v, n) -> (n, integer2Name (toInteger v))) $ getQuantorMap tps
        tyvars = map (\x -> (TVar x, integer2Name (toInteger x))) $ quantifiers tps
        qs :: [Predicate]
        (qs, tp) = split $ unquantify tps
        monoType = tpToMonoType fams tp
        convertPred :: Predicate -> Constraint
        convertPred (Predicate c v) = case lookup v tyvars of
            Nothing -> internalError "TopConversion" "tpSchemeToMonoType" "Type variable not found"
            Just tv -> Constraint_Class c [var tv] Nothing
        in (map convertPred qs , qmap, monoType)

tpToMonoType :: TypeFamilies -> Tp -> MonoType
tpToMonoType fams (TVar v) = var (integer2Name $ toInteger v)
tpToMonoType fams (TCon n) | isTypeFamily fams (TCon n) = MonoType_Fam n []
                           | otherwise = MonoType_Con n
tpToMonoType fams ta@(TApp f a) | isTypeFamily fams ta = let 
                                                m1 = tpToMonoType fams f
                                                m2 = tpToMonoType fams a 
                                                (MonoType_Con famName, params) = conList (MonoType_App m1 m2)
                                                in MonoType_Fam famName params
                                | otherwise = MonoType_App (tpToMonoType fams f) (tpToMonoType fams a)

tpDepth :: Tp -> Int
tpDepth (TVar _) = 0
tpDepth (TCon _) = 0
tpDepth (TApp f _) = 1 + tpDepth f

tpCons :: Tp -> Maybe String
tpCons (TVar _) = Nothing
tpCons (TCon n) = Just n
tpCons (TApp f _) = tpCons f

isTypeFamily :: TypeFamilies -> Tp -> Bool
isTypeFamily fams tp = let
    depth = tpDepth tp
    fFams = filter (\x -> snd x == depth) fams
    cons = tpCons tp
    in any (\(x, _) -> Just x == cons) fFams

getTypeVariablesFromPolyType :: PolyType -> [TyVar]
getTypeVariablesFromPolyType (PolyType_Bind _ (B p t)) = p : getTypeVariablesFromPolyType t
getTypeVariablesFromPolyType _ = []

getTypeVariablesFromPolyType' :: PolyType -> [TyVar]
getTypeVariablesFromPolyType' (PolyType_Mono _ m) = fv m
getTypeVariablesFromPolyType' _ = []

getTypeVariablesFromMonoType :: MonoType -> [TyVar]
getTypeVariablesFromMonoType (MonoType_Var _ v) = [v]
getTypeVariablesFromMonoType (MonoType_Fam _ ms) = nub $ concatMap getTypeVariablesFromMonoType ms
getTypeVariablesFromMonoType (MonoType_Con _) = []
getTypeVariablesFromMonoType (MonoType_App f a) = nub $ getTypeVariablesFromMonoType f ++ getTypeVariablesFromMonoType a

getTypeVariablesFromConstraints :: Constraint -> [TyVar]
getTypeVariablesFromConstraints (Constraint_Unify v1 v2 _) = nub $ getTypeVariablesFromMonoType v1 ++ getTypeVariablesFromMonoType v2
getTypeVariablesFromConstraints (Constraint_Class _ vs _) = nub $ concatMap getTypeVariablesFromMonoType vs

getConstraintFromPoly :: PolyType -> [Constraint]
getConstraintFromPoly (PolyType_Bind _ (B _ t)) = getConstraintFromPoly t
getConstraintFromPoly (PolyType_Mono cs _) = cs

polytypeToMonoType :: [(Integer, Integer)] -> Integer -> PolyType -> ([(Integer, Integer)], ((MonoType, [Constraint]), Integer))
polytypeToMonoType mapping bu (PolyType_Bind s b) = let
    ((_, x), bu') = contFreshMRes (unbind b) bu
    in polytypeToMonoType mapping bu' x
polytypeToMonoType mapping bu (PolyType_Mono cs m) = freshenWithMapping mapping bu (m, cs)
    
classEnvironmentToAxioms :: TypeFamilies -> ClassEnvironment -> [Axiom]
classEnvironmentToAxioms fams env = concatMap (uncurry classToAxioms) (M.toList env)
    where
        classToAxioms :: String -> Class -> [Axiom]
        classToAxioms s (superclasses, instances) = map instanceToAxiom instances
        instanceToAxiom :: Instance -> Axiom
        instanceToAxiom ((Predicate cn v), supers) = let
                vars = map (integer2Name  . toInteger) (ftv v ++ concatMap (\(Predicate _ v) -> ftv v) supers)
                superCons = map (\(Predicate c v) -> Constraint_Class c [tpToMonoType fams v] Nothing) supers
            in Axiom_Class (bind vars (superCons, cn, [tpToMonoType fams v]))

           -- type TypeSynonymEnvironment      = M.Map Name (Int, Tps -> Tp)

typeSynonymsToAxioms :: TypeSynonymEnvironment -> [Axiom]
typeSynonymsToAxioms env = concatMap tsToAxioms $ M.toList env
            where
                tsToAxioms (name, (size, f)) = let
                        fams = map (\(n, (i, _)) -> (show n, i)) $ M.assocs  env
                        vars = take size [0..]
                        tpVars = map TVar vars
                        tp = f tpVars
                        mt = tpToMonoType fams tp
                        mtVars = map (integer2Name . toInteger) vars
                        
                        unifyAxiom = Axiom_Unify (bind mtVars ((MonoType_Fam (show name) $ map var mtVars), mt))
                    in [unifyAxiom] -- [Axiom_Injective $ show name, unifyAxiom]



instance Freshen MonoType Integer where
    freshenWithMapping mapping n mt = (\(mt', (n', m'))->(map (name2Integer *** name2Integer) m', (mt', n'))) $ 
        runState (freshenHelperMT mt) (n, map (integer2Name *** integer2Name) mapping) 
        
freshenHelperMT :: MonoType -> State (Integer, [(TyVar, TyVar)]) MonoType
freshenHelperMT (MonoType_Var _ v') =  
    do
        (uniq, mapping) <- get
        case lookup v' mapping of
            Just v -> return (var v)
            Nothing -> put (uniq + 1, (v', integer2Name uniq) : mapping) >> return (var $ integer2Name uniq)
freshenHelperMT c@(MonoType_Con _) = return c
freshenHelperMT  (MonoType_App f a) = do
    f' <- freshenHelperMT f
    a' <- freshenHelperMT a
    return (MonoType_App f' a')
freshenHelperMT (MonoType_Fam s xs) = do
    (n, mapping) <- get
    let (mapping', (xs', n')) = freshenWithMapping (map (name2Integer *** name2Integer) mapping) n xs
    put (n', map (integer2Name *** integer2Name) mapping')
    return (MonoType_Fam s xs')

instance Freshen PolyType Integer where
    freshenWithMapping mapping n mt = (\(mt', (n', m'))->(map (name2Integer *** name2Integer) m', (mt', n'))) $ 
        runState (freshenHelper mt) (n, map (integer2Name *** integer2Name) mapping) 
        where
            freshenHelper :: PolyType -> State (Integer, [(TyVar, TyVar)]) PolyType
            freshenHelper (PolyType_Mono cs m) = do
                m' <- freshenHelperMT m
                (uniq, mapping) <- get
                let cs' = map (substs (map (\(t, v) -> (t, var v)) mapping)) cs
                return (PolyType_Mono cs' m')
            freshenHelper (PolyType_Bind s b) = do
                (uniq, mapping) <- get
                let ((p, t), uniq') = contFreshMRes (unbind b) uniq
                let p' = integer2Name $ uniq' + 1
                put (uniq' + 2, (p, p') : mapping)
                t' <- freshenHelper t
                return (PolyType_Bind s (bind p' t'))

instance Freshen TyVar Integer where
    freshenWithMapping mapping n v = let 
        vi = name2Integer v
        in case lookup vi mapping of
                Nothing -> ((vi, n) : mapping, (integer2Name n, n + 1))
                Just v' -> (mapping, (integer2Name v', n))

instance Freshen Char Integer where
    freshenWithMapping mapping n c = (mapping, (c, n))

instance (Freshen a c, Freshen b c) => Freshen (a, b) c where
    freshenWithMapping mapping n (x, y) = let
        (mapping', (x', b)) = freshenWithMapping mapping n x
        (mapping'', (y', b')) = freshenWithMapping mapping' b y
        in (mapping'', ((x', y'), b')) 

instance (Freshen a d, Freshen b d, Freshen c d) => Freshen (a, b, c) d where
    freshenWithMapping mapping n (x, y, z) = let
        (mapping', (x', b)) = freshenWithMapping mapping n x
        (mapping'', (y', b')) = freshenWithMapping mapping' b y
        (mapping''', (z', b'')) = freshenWithMapping mapping'' b' z
        in (mapping'', ((x', y', z'), b''))         


instance Freshen Constraint Integer where
    freshenWithMapping mapping n (Constraint_Class cn vs ci) = let 
        (mapping', (vs', n')) = freshenWithMapping mapping n vs
        in (mapping', (Constraint_Class cn vs' ci, n'))
    freshenWithMapping mapping n (Constraint_Unify v1 v2 ci) = let
        (mapping', (v1', n')) = freshenWithMapping mapping n v1
        (mapping'', (v2', n'')) = freshenWithMapping mapping' n' v2
        in (mapping'', (Constraint_Unify v1' v2' ci, n''))


contFreshMRes :: FreshM a -> Integer -> (a, Integer)
contFreshMRes i = runIdentity . contFreshMTRes i

contFreshMTRes :: Monad m => FreshMT m a -> Integer -> m (a, Integer)
contFreshMTRes (FreshMT m) = runStateT m

unbindPolyType :: PolyType -> PolyType
unbindPolyType = runFreshM . unbindPolyType'

unbindPolyType' :: PolyType -> FreshM PolyType
unbindPolyType' (PolyType_Bind s b) = snd <$> unbind b >>= unbindPolyType'
unbindPolyType' pt = return pt