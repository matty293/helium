module Helium.CodeGeneration.Core.Strictness.MonovariantSimple (monovariantStrictness) where

-- THIS ANALYSIS IS DEPRECATED

import Data.List
import qualified Data.Set as S

import Helium.CodeGeneration.Core.BindingGroup
import Helium.CodeGeneration.Core.Strictness.Data
import Helium.CodeGeneration.Core.Strictness.DataSimple
import Helium.CodeGeneration.Core.Strictness.Solve
import Helium.CodeGeneration.Core.TypeEnvironment

import Lvm.Common.Id
import Lvm.Common.IdMap
import Lvm.Core.Expr
import Lvm.Core.Module
import Lvm.Core.Type

import Text.PrettyPrint.Leijen (pretty)

-- Analysis data type, containing return expression, set of constraints and the environment containing the annotations
data Analysis a = Analysis a Constraints AnnotationEnvironment BindMap

mergeAnalysis :: (SAnn -> SAnn -> SAnn) -> [Analysis a] -> Analysis [a]
mergeAnalysis _ [] = Analysis [] S.empty emptyMap emptyMap
mergeAnalysis f (x:xs) = Analysis (v:v') (S.union c c') (unionMapWith f a a') (unionMap r r')
    where
        Analysis v c a r = x
        Analysis v' c' a' r' = mergeAnalysis f xs

type GroupData = (MonoMap, SolvedConstraints, BindMap, TypeEnvironment, NameSupply)
type CoreGroup = BindingGroup Expr

monovariantStrictness :: NameSupply -> CoreModule -> CoreModule
monovariantStrictness supply m = m{moduleDecls = values''}
  where
    (supply1, supply2) = splitNameSupply supply
    -- Ignore declarations which have already been analysed
    (decls1, decls2) = partition (any isCustomAnn . declCustoms) $ moduleDecls m
    -- Split module in functions and others (constructors, abstract, synonyms)
    (values, others) = partition isDeclValue decls2
    -- For declarations which have been annotated, set strictness type to declType
    decls1' = map setStrictnessType decls1
    -- Annotate others
    others' = mapWithSupply (annotateDeclaration (typeEnvForModule m)) supply1 others
    -- Create starting environment
    env = typeEnvForModule m{moduleDecls = others' ++ decls1'}
    -- Binding group analysis for functions
    groups = coreBindingGroups values
    (values', sc, r, _, _) = foldl groupStrictness (emptyMap, emptyMap, emptyMap, env, supply2) groups
    -- Transform declarations based on solved constraints
    values'' = map (transformDeclaration (annotateTypeAbstract env) values' sc r) $ moduleDecls m

groupStrictness :: GroupData -> CoreGroup -> GroupData
-- Single declaration
groupStrictness (v, sc, r, env, supply) (BindingNonRecursive d) = (v', sc'', r'', env', supply2)
  where
    (supply1, supply2) = splitNameSupply supply
    -- Analyse function
    Analysis e cs ae r' = analyseDeclaration env supply1 d
    -- Solve constraints
    sc' = solveConstraints ae cs
    -- Get annotated type of body
    t = transformType sc' $ normalTypeOfCoreExpression env e
    -- Add strictness type to environment
    env' = typeEnvAddGlobalValue (declName d) t env
    -- Update values for next binding group
    v' = insertMap (declName d) t v 
    sc'' = unionMapWith join sc sc'
    r'' = unionMap r r'
-- Group of recursive declarations
groupStrictness (v, sc, r, env, supply) (BindingRecursive ds) = (v', sc'', r'', env'', supply3)
  where
    (supply1, supply') = splitNameSupply supply
    (supply2, supply3) = splitNameSupply supply'
    -- Annotate type signatures and add them to the environment
    ts = mapWithSupply (\s d -> annotateType env s $ declType d) supply1 ds
    env' = typeEnvAddGlobalValues (map (\(d, t) -> (declName d, t)) (zip ds ts)) env
    -- Run analysis on all bodies, merge information with meet
    Analysis es cs ae r' = mergeAnalysis meet $ mapWithSupply (analyseDeclaration env') supply2 ds
    -- Get type of function bodies
    ts' = map (normalTypeOfCoreExpression env') es
    -- Analyse type to solve the annotations which where put there at the beginning
    (ae2, cs2) = unzip $ zipWith (analyseType env) ts ts'  
    -- Solve constraints
    sc' = solveConstraints (unionMapWith join ae (unionMaps ae2)) (S.union cs (S.unions cs2))
    -- Apply solved constraints to types
    ts'' = map (transformType sc) ts'
    -- Add strictness types to environment
    env'' = typeEnvAddGlobalValues (map (\(d, t) -> (declName d, t)) (zip ds ts'')) env
    -- Update values for next binding group
    v' = foldl (\v'' (d, t) -> insertMap (declName d) t v'') v (zip ds ts'')
    sc'' = unionMapWith join sc sc'
    r'' = unionMap r r'
groupStrictness d (BindingNonFunction _) = d -- Split occurs before binding group analysis

{-
    Analyse
-}

analyseDeclaration :: TypeEnvironment -> NameSupply -> CoreDecl -> Analysis Expr
analyseDeclaration tyEnv supply decl@DeclValue{} = analyseExpression env S 0 supply $ valueValue decl
    where
        env = Environment tyEnv emptyMap

analyseExpression :: Environment -> SAnn -> Int -> NameSupply -> Expr -> Analysis Expr
analyseExpression env rel app supply (Let b e) = Analysis (Let b' e') (S.union c1 c2) (unionMapWith meet a1 as) (unionMap r1 r2)
    where
        (supply1, supply2) = splitNameSupply supply
        -- Analyse binds
        Analysis b' c1 a1 r1 = analyseBinds env supply1 rel b
        -- Add annotated binds to environment
        env' = envAddBinds b' env
        -- Analyse body, set contexts to S
        Analysis e' c2 a2 r2 = analyseExpression env' S app supply2 e
        -- Containment on old environment
        as = containment env rel a2
analyseExpression env rel app supply (Match i a) = Analysis (Match i a') c ae r
    where
        -- Merge with join as strictness has to occur in every case
        Analysis a' c ae r = mergeAnalysis join $ mapWithSupply (analyseAlt env rel app i) supply a
analyseExpression env rel app supply (Ap e1 e2) = Analysis (Ap e1' e2') (S.unions [c1, c2, c3]) (unionMapsWith meet [ae1, ae2, ae3']) (unionMap r1 r2)
    where
        (supply1, supply2) = splitNameSupply supply
        -- Analyse function, with applicative counter incremented
        Analysis e1' c1 ae1 r1 = analyseExpression env rel (app + 1) supply1 e1
        -- Get type of function
        t = normalTypeOfCoreExpression (typeEnv env) e1'
        -- Get the annotation on the function arrow
        (TAp (TAp (TCon TConFun) (TAp (TAnn r) t')) _) = t
        -- If not fully applied, set context to L
        rel' = if arityFromType t == app + 1 then join rel r else L
        -- Analyse argument with relevance annotation per above, applicative context reset
        Analysis e2' c2 ae2 r2 = analyseExpression env rel' 0 supply2 e2
        -- Annotation unifications between the function and the given argument
        (ae3, c3) = analyseType (typeEnv env) t' $ normalTypeOfCoreExpression (typeEnv env) e2'
        -- Containment for datatype annotations
        ae3' = mapMap (join rel) ae3
analyseExpression env rel app supply (ApType e t)
    | isTupAp e = Analysis (ApType e' tt) c a r
    | isNil e = Analysis (ApType e' tn) c a r
    | otherwise = Analysis (ApType e' t') c a r
    where
        (supply1, supply2) = splitNameSupply supply
        -- Annotate type, if it is a tuple place extra annotation
        tt = annotateVarType env supply1 t
        -- If empty list, annotate as best as possible
        tn = annotateTypeMax (typeEnv env) t
        -- Annotate normally
        t' = annotateType (typeEnv env) supply1 t
        -- Analyse expression
        Analysis e' c a r = analyseExpression env rel app supply2 e
analyseExpression env rel _ supply (Lam s (Variable x t) e) = Analysis (Lam s v' e') c a' r'
    where
        (i, supply') = freshId supply
        (supply1, supply2) = splitNameSupply supply'
        -- Annotate type in variable
        t' = annotateType (typeEnv env) supply1 t
        -- If lambda was strict, set its annotation variables equal to S
        ann = if s then S else AnnVar i
        -- Give extra annotation to variable
        v' = Variable x (TAp (TAnn ann) t')
        -- Add variable to environment
        env' = envAddVariable v' env
        -- Analyse expression, set relevance to S and context to 0
        Analysis e' c a r = analyseExpression env' S 0 supply2 e
        -- Containment on old environment
        a' = containment env rel a
        -- Contain if function because we don't know if it is applied
        ae = case arityFromType t' of
            0 -> a'
            _ -> a' --mapMap (join L) a
        -- If not strict, add variable to map which might turn to strict
        r' = if s then r else insertMap x (AnnVar i) r
analyseExpression env rel app supply (Forall q k e) = Analysis (Forall q k e') c a r
    where
        -- Forall can be ignored
        Analysis e' c a r = analyseExpression env rel app supply e
analyseExpression env _ _ _ (Con c) = Analysis (Con c) S.empty (getLConstraints env) emptyMap -- Set all annotation variables to L
analyseExpression env rel _ _ (Var v) = Analysis (Var v) S.empty (unionMapWith meet (getLConstraints env) ae) emptyMap
    where
        -- Set all annotation variables to L except the annotations related to this variable, which are set to context
        ae = getAnnotations env rel v
analyseExpression env _ _ _ (Lit l) = Analysis (Lit l) S.empty (getLConstraints env) emptyMap -- Set all annotation variables to L

analyseBinds :: Environment -> NameSupply -> SAnn -> Binds -> Analysis Binds
analyseBinds env supply rel (Rec bs) = Analysis (Rec bs'') c a r
    where
        -- Annotate types beforehand because they occur in the body
        bs' = mapWithSupply (annotateBind env) supply bs
        -- Add binds to environment
        env' = envAddBinds (Rec bs') env
        -- Run analysis on every bind separately
        xs = mapWithSupply (analyseRecBind env' rel) supply bs'
        -- Merge the results with meet, as being strict in one bind is enough
        Analysis bs'' c a r = mergeAnalysis meet xs
analyseBinds env supply rel (NonRec (Bind (Variable x _) e)) = Analysis (NonRec b) cs ae' r'
    where
        -- Fresh variable for relevance annotation
        (i, supply') = freshId supply
        -- Run analysis on binding with relevance set to context
        Analysis e' cs ae r = analyseExpression env (join rel (AnnVar i)) 0 supply' e
        -- Get type of bind to store in signature
        t' = normalTypeOfCoreExpression (typeEnv env) e'
        -- Contain if function because we don't know if it is applied
        ae' = case arityFromType t' of
            0 -> ae
            _ -> ae --mapMap (join L) ae
        -- Add annotations outside the type
        b = Bind (Variable x (TAp (TAnn $ AnnVar i) t')) e'
        -- Bind is NonRec, add to map of those which might be turned to strict
        r' = insertMap x (AnnVar i) r
analyseBinds env supply rel (Strict (Bind (Variable x _) e)) = Analysis (Strict b) cs ae' r
    where
        -- Run analysis on binding with relevance set to context
        Analysis e' cs ae r = analyseExpression env rel 0 supply e
        -- Get type of bind to store in signature
        t' = normalTypeOfCoreExpression (typeEnv env) e'
        -- Contain if function because we don't know if it is applied
        ae' = case arityFromType t' of
            0 -> ae
            _ -> ae --mapMap (join L) ae
        -- Add annotations outside the type
        b = Bind (Variable x (TAp (TAnn S) t')) e'

analyseRecBind :: Environment -> SAnn -> NameSupply -> Bind -> Analysis Bind
analyseRecBind env rel supply (Bind v e) = Analysis (Bind (Variable x (TAp (TAnn rel') t')) e') cs ae r
    where
        -- Get annotations from variable previously annotated
        Variable x (TAp (TAnn rel') t) = v
        -- Run analysis on binding with relevance set to context
        Analysis e' cs1 ae1 r = analyseExpression env (join rel rel') 0 supply e
        -- Get type of body
        t' = normalTypeOfCoreExpression (typeEnv env) e'
        -- Contain if function because we don't know if it is applied
        ae1' = case arityFromType t' of
            0 -> ae1
            _ -> ae1 --mapMap (join L) ae1
        -- As recursive binding, find the new values for the placed annotations
        (ae2, cs2) = analyseType (typeEnv env) t t'
        cs = S.union cs1 cs2
        ae = unionMapWith join ae1' ae2
        
analyseAlt :: Environment -> SAnn -> Int -> Id -> NameSupply -> Alt -> Analysis Alt
analyseAlt env rel app i supply (Alt p e) = Analysis (Alt p' e') (S.union c1 c2) (unionMapWith meet a1' a2) r
    where
        (supply1, supply2) = splitNameSupply supply
        -- Analyse the pattern
        Analysis p' c1 a1 _ = analysePat env i supply1 p
        -- Containment for datatype annotations
        a1' = mapMap (join rel) a1
        -- Add pattern to environment
        env' = envAddPattern p' env
        -- Run analysis 
        Analysis e' c2 a2 r = analyseExpression env' rel app supply2 e

analysePat :: Environment -> Id -> NameSupply -> Pat -> Analysis Pat
analysePat env i' supply (PatCon (ConTuple n) t i) = Analysis (PatCon (ConTuple n) t' i) cs ae emptyMap
    where
        -- In case of a tuple, all types need an extra annotation to communicate the return annotation of the tuple
        t' = mapWithSupply (annotateVarType env) supply t
        -- Get equalities between type of id matched on and type of pattern
        (ae, cs) = analyseType (typeEnv env) (foldl TAp (TCon (TConTuple n)) t') (typeOfId (typeEnv env) i')
analysePat env i' supply (PatCon c t i) = Analysis (PatCon c t' i) cs ae emptyMap
    where
        t' | isNil (Con c) = map (annotateTypeMax (typeEnv env)) t -- Empty list, annotate as best as possible
           | otherwise = mapWithSupply (annotateType (typeEnv env)) supply t -- Annotate all types given to constructor
        -- Add pattern to environment
        env' = envAddPattern (PatCon c t' i) env
        -- Construct expression equivalent to constructor
        e = foldl Ap (foldl ApType (Con c) t') (map Var i)
        -- Analyse type of matched id with type of constructor
        (ae, cs) = analyseType (typeEnv env) (normalTypeOfCoreExpression (typeEnv env') e) (typeOfId (typeEnv env) i')
analysePat _ _ _ p = Analysis p S.empty emptyMap emptyMap -- Literal or default, no information to be gained

-- Analyse type
analyseType :: TypeEnvironment -> Type -> Type -> (AnnotationEnvironment, Constraints)
analyseType env t1 t2
    | t1 /= t1' || t2 /= t2' = analyseType env t1' t2' -- Normalization changes types, try again
    | t1 == t2               = (emptyMap, S.empty) -- Types equal, analysis completed
        where
            t1' = typeNormalizeHead env t1
            t2' = typeNormalizeHead env t2
analyseType env (TAp (TAp (TCon TConFun) t11) t12) (TAp (TAp (TCon TConFun) t21) t22) = (unionMapWith join ae1 ae2, cs)
    -- Function arrows, analyse the pair of annotations on them
  where
    (TAp (TAnn a ) t1') = t11
    (TAp (TAnn a') t2') = t21
    (ae1, c1) = analyseType env t1' t2'
    (ae2, c2) = analyseType env t12 t22
    cs = S.insert (a' `Constraint` a) $ S.union c1 c2
analyseType env (TAp t11 t12) (TAp t21 t22) = (unionMapWith join ae1 ae2, S.union c1 c2)
    -- Unannotated applications
    where
        (ae1, c1) = analyseType env t11 t21
        (ae2, c2) = analyseType env t12 t22
analyseType env (TForall _ _ t1) (TForall _ _ t2) = analyseType env t1 t2
analyseType env (TStrict t1) t2 = analyseType env t1 t2
analyseType env t1 (TStrict t2) = analyseType env t1 t2 -- Remove all strict type information
analyseType _ (TVar _) (TVar _) = (emptyMap, S.empty) -- Lift has a bug which might distort type variables, exact index doesn't matter
analyseType _ t1 t2 = error $ "analyseType: type mismatch: " ++ show (pretty t1) ++ " and " ++ show (pretty t2)

{-
    Annotate
-}

annotateDeclaration :: TypeEnvironment -> NameSupply -> CoreDecl -> CoreDecl
annotateDeclaration env _ decl@DeclAbstract{} = decl{declType = annotateTypeAbstract env (declType decl)}
annotateDeclaration env _ decl@DeclCon{}
    | declName decl == idFromString "[]" = decl{declType = annotateNil (declType decl)}
    | declName decl == idFromString ":"  = decl{declType = annotateCons (declType decl)}
    | otherwise = decl{declType = annotateTypeAbstract env (declType decl)}
annotateDeclaration env supply decl@DeclTypeSynonym{}
    -- String is the only type synonym which has to be annotated because it is partly hardcoded in the type system
    | declName decl == idFromString "String" = decl{declType = annotateType env supply (declType decl)}
annotateDeclaration _ _ decl = decl -- Value is handled outside this method, others don't need anything


-- [a] to S [a]
annotateNil :: Type -> Type
annotateNil (TForall q k (TAp t1 t2)) = TForall q k $ TAp (TAp (TAnn S) t1) t2

-- a -> [a] -> [a] to a -L> L [a] -L> L [a]
annotateCons :: Type -> Type
annotateCons (TForall q k t) = TForall q k ta
  where
    TAp (TAp (TCon TConFun) t1) t' = t
    TAp (TAp (TCon TConFun) (TAp l1 t2)) (TAp l2 t3) = t'
    ta = TAp (TAp (TCon TConFun) (TAp (TAnn L) t1)) ta'
    t2' = TAp (TAnn L) (TAp (TAp (TAnn L) l1) t2)
    t3' = TAp (TAp (TAnn L) l2) t3
    ta' = TAp (TAp (TCon TConFun) t2') t3'

{-
    Transform
-}

-- Apply strict annotations on declarations
transformDeclaration :: (Type -> Type) -> MonoMap -> SolvedConstraints -> BindMap -> CoreDecl -> CoreDecl
transformDeclaration _ vs sc r decl@DeclValue{}
    | elemMap (declName decl) vs = decl{valueValue = e, declCustoms = c}
    where
        t = findMap (declName decl) vs
        e = transformExpression sc r $ valueValue decl
        c = strictnessToCustom (transformType sc t) $ declCustoms decl
transformDeclaration f _ _ _ decl
  | isUpdate decl = decl{declCustoms = c}
  | otherwise     = decl
    where
      c = strictnessToCustom (f $ declType decl) (declCustoms decl)

-- Apply strict annotations on types
transformType :: SolvedConstraints -> Type -> Type
transformType sc (TAp (TAp (TCon TConFun) (TAp (TAnn a) t1)) t2) =
  TAp (TAp (TCon TConFun) t1') t2'
    where
      a' = lookupVarMono a sc
      t1' = TAp (TAnn a') (transformType sc t1)
      t2' = transformType sc t2
transformType sc (TAp t1 t2) = TAp (transformType sc t1) (transformType sc t2)
transformType sc (TAnn a) = TAnn $ lookupVarMono a sc
transformType sc (TStrict t) = transformType sc t
transformType sc (TForall q k t) = TForall q k $ transformType sc t
transformType _ t = t

-- Apply strict annotations on expressions
transformExpression :: SolvedConstraints -> BindMap -> Expr -> Expr
transformExpression sc r (Let b e) = Let (transformBinds sc r b) $ transformExpression sc r e
transformExpression sc r (Match i as) = Match i $ map transformAlt as
    where
        transformAlt (Alt p e) = Alt p $ transformExpression sc r e
transformExpression sc r (Ap e1 e2) = Ap e1' e2'
  where
    e1' = transformExpression sc r e1
    e2' = transformExpression sc r e2
transformExpression sc r (ApType e t) = ApType (transformExpression sc r e) t
transformExpression sc r (Lam s v@(Variable x _) e) = Lam s' v e'
  where
    s' = if s then s else lookupVar (findMap x r) sc == S
    e' = transformExpression sc r e
transformExpression sc r (Forall q k e) = Forall q k $ transformExpression sc r e
transformExpression _ _ e = e -- Con, Lit and Var

-- Apply strict transformations on binds
transformBinds :: SolvedConstraints -> BindMap -> Binds -> Binds
transformBinds sc r (Strict (Bind v e)) = Strict $ Bind v (transformExpression sc r e)
transformBinds sc r (NonRec (Bind v@(Variable x _) e)) = b'
  where
    b' = case lookupVar (findMap x r) sc of
        S -> Strict $ Bind v e'
        _ -> NonRec $ Bind v e'
    e' = transformExpression sc r e
transformBinds sc r (Rec bs) = Rec $ map transformBind bs
    where
        transformBind (Bind v e) = Bind v $ transformExpression sc r e