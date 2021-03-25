module Helium.CodeGeneration.Iridium.Region.PassRegion where

import Lvm.Common.Id
import Lvm.Common.IdMap
import Lvm.Core.Type

import Helium.CodeGeneration.Core.TypeEnvironment

import Helium.CodeGeneration.Iridium.Data
import Helium.CodeGeneration.Iridium.BindingGroup

import Helium.CodeGeneration.Iridium.Region.Env
import Helium.CodeGeneration.Iridium.Region.Generate
import Helium.CodeGeneration.Iridium.Region.Sort
import Helium.CodeGeneration.Iridium.Region.Annotation
import Helium.CodeGeneration.Iridium.Region.Evaluate
import Helium.CodeGeneration.Iridium.Region.RegionVar
import Helium.CodeGeneration.Iridium.Region.Relation
import Helium.CodeGeneration.Iridium.Region.Transform
import Helium.CodeGeneration.Iridium.Region.DeadRegion
import Helium.CodeGeneration.Iridium.Region.Utils

passRegion :: NameSupply -> Module -> IO Module
passRegion supply m = do
  let genv = initialEnv m
  let groups = methodBindingGroups $ moduleMethods m

  (_, methods) <- mapAccumLM transformGroup genv groups

  return m{ moduleMethods = concat methods }

initialEnv :: Module -> GlobalEnv
initialEnv m = GlobalEnv typeEnv dataTypeEnv functionEnv
  where
    -- Environment is only used for type synonyms
    typeEnv = TypeEnvironment (mapFromList synonyms) emptyMap emptyMap

    dataTypeEnv :: DataTypeEnv
    dataTypeEnv = mapFromList $ map dataTypeSort $ moduleDataTypes m

    functionEnv :: IdMap (Int, Annotation)
    functionEnv = mapFromList $ methods ++ abstracts

    dataTypeSort :: Declaration DataType -> (Id, DataTypeSort)
    dataTypeSort (Declaration name _ _ _ (DataType _)) = (name, DataTypeSort relationEmpty SortUnit RegionSortUnit)

    abstracts :: [(Id, (Int, Annotation))]
    abstracts = abstract <$> moduleAbstractMethods m

    abstract :: Declaration AbstractMethod -> (Id, (Int, Annotation))
    abstract (Declaration name _ _ _ (AbstractMethod tp regionSort _ annotations)) = (name, (regionSortSize regionSort, regionAnnotation tp annotations))

    methods :: [(Id, (Int, Annotation))]
    methods = method <$> moduleMethods m

    method :: Declaration Method -> (Id, (Int, Annotation))
    method (Declaration name _ _ _ (Method tp _ _ _ _ _ _ _)) = (name, (0, top tp))

    top :: Type -> Annotation
    top = ATop . SortFun SortUnit RegionSortUnit LifetimeContextAny . sortOfType dataTypeEnv . typeNormalize typeEnv

    synonyms :: [(Id, Type)]
    synonyms = [(name, tp) | Declaration name _ _ _ (TypeSynonym _ tp) <- moduleTypeSynonyms m]

    regionAnnotation :: Type -> [MethodAnnotation] -> Annotation
    regionAnnotation tp [] = top tp
    regionAnnotation tp (a:as)
      | MethodAnnotateRegion r <- a = r
      | otherwise = regionAnnotation tp as

-- Analyses and transforms a binding group of a single non-recursive function
-- or a group of (mutual) recursive functions.
transformGroup :: GlobalEnv -> BindingGroup Method -> IO (GlobalEnv, [Declaration Method])
transformGroup genv (BindingRecursive methods) = do
  -- We cannot analyse mutual recursive functions yet
  -- For now we will analyse them one by one.
  -- TODO: This will cause issues when later functions in the binding group get additional region arguments.
  (genv'', methods') <- mapAccumLM (\genv' method -> transformGroup genv' $ BindingNonRecursive method) genv methods
  return (genv'', concat methods')

transformGroup genv@(GlobalEnv typeEnv dataTypeEnv globals) (BindingNonRecursive method@(Declaration methodName _ _ _ (Method _ _ arguments _ _ _ _ _))) = do
  -- putStrLn $ "# Analyse method " ++ show methodName

  let (methodEnv, annotation) = generate genv method
  -- print annotation

  let (doesEscape, substituteRegionVar, simplified) = simplifyFixEscape dataTypeEnv annotation
  -- putStrLn "Simplified:"
  -- print simplified

  let (isZeroArity, simplified') = correctArityZero (methodEnvAdditionalRegionSort methodEnv) arguments simplified
  let simplified'' = if isZeroArity then simplify dataTypeEnv simplified' else simplified'

  let (_, restricted) = if isZeroArity then (0, simplified'') else annotationRestrict doesEscape simplified

  -- putStrLn "Restricted:"
  -- print doesEscape
  -- print restricted

  let (regionCount, method', restricted') = transformDead restricted $ transform methodEnv isZeroArity substituteRegionVar restricted $ declarationValue method

  rnfAnnotation restricted' `seq` return ()

  let globals' = updateMap methodName (regionCount, restricted') globals
  let genv' = GlobalEnv typeEnv dataTypeEnv globals'

  return (genv', [method{ declarationValue = method' }])