{-# LANGUAGE PatternSynonyms #-}

module Helium.CodeGeneration.Iridium.RegionSize.Annotation
  ( Annotation(..), Effect, pattern AUnit, annShow,
    AnnAlg(..), foldAnnAlg, foldAnnAlgN, idAnnAlg,
    liftTuple, unliftTuple,
    collect,
    annWeaken, annStrengthen,
    isConstr, constrIdxToAnn,
    annRemLocalRegs
  ) where

import Helium.CodeGeneration.Iridium.Region.RegionVar

import Helium.CodeGeneration.Iridium.RegionSize.Constraints
import Helium.CodeGeneration.Iridium.RegionSize.Sort
import Helium.CodeGeneration.Iridium.RegionSize.Utils
import Helium.CodeGeneration.Iridium.RegionSize.Type

import qualified Data.Map as M
import Data.List
import Lvm.Core.Type

----------------------------------------------------------------
-- Annotation data type
----------------------------------------------------------------

data Annotation = 
      AVar    Int                     -- ^ De Bruijn i7ndex Variable
    | AReg    RegionVar               -- ^ Region
    | ALam    Sort       Annotation   -- ^ Annotation lambda
    | AApl    Annotation Annotation   -- ^ Application
    | AConstr Constr                  -- ^ Constraint set
    | ATuple  [Annotation]            -- ^ Unit tuple
    | AProj   Int        Annotation   -- ^ Projection
    | AAdd    Annotation Annotation   -- ^ Constraint set addition
    | AMinus  Annotation RegionVar    -- ^ Constraint set minus
    | AJoin   Annotation Annotation   -- ^ Annotation join
    | AQuant  Annotation              -- ^ Quantification
    | AInstn  Annotation Type         -- ^ Insantiation of quantification
    | ATop    Sort       Constr       -- ^ Has a constraint set, all bounds should be infty
    | ABot    Sort  
    | AFix    Sort       [Annotation] -- ^ Fix point has a list of (possibly mutally recursive) annotations
  deriving (Eq, Ord)

-- | The effect is an annotation, but always of sort C
type Effect = Annotation

-- | AUnit is a 0-tuple, a patern disallows them from co-existing
pattern AUnit :: Annotation
pattern AUnit = ATuple []

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

instance Show Annotation where
  show = cleanTUP . annShow (-1)

annShow :: Int -> Annotation -> String    
annShow n = foldAnnAlgN n showAlg
     where showAlg = AnnAlg {
        aVar    = \d idx -> annVarName (d - idx),
        aReg    = \_ idx -> show idx,
        aLam    = \d s a -> "(λ"++ annVarName (d+1) ++":"++ showSort d s ++ ".\n" ++ indent "  " a ++ ")",
        aApl    = \_ a b -> a ++ "<" ++ indent " "  b ++ " >",
        aUnit   = \_     -> "TUP()",
        aTuple  = \_ as  -> "TUP(" ++ intercalate (if noTupleBreak (as !! 0) then "," else "\n,") as ++ ")",
        aProj   = \_ i a -> "π_" ++ show i ++ "[" ++ a ++ "]",
        aAdd    = \_ a b -> "(" ++ a ++ " ⊕  " ++ b ++ ")",
        aMinus  = \_ a r -> "(" ++ a ++ " \\ " ++ show r ++ ")",
        aJoin   = \_ a b -> "(" ++ a ++ " ⊔  " ++ b ++ ")",
        aQuant  = \d a   -> "(∀ " ++ typeVarName (d+1) ++ "." ++ a ++ ")",
        aInstn  = \d a t -> a ++ " {" ++ showTypeN d t ++ "}",
        aTop    = \d _ c -> "T"  ++ "[" ++ (constrShow d c) ++ "]",
        aBot    = \_ _   -> "⊥",
        aFix    = \d s a -> "fix " ++ annVarName (d+1) ++ " : " ++ showSort d s 
                                   ++ ".\n[" ++ (intercalate ",\n" $ mapWithIndex (\i str -> show i ++ ": " ++ str) $ indent "  " <$> a) ++ "]",
        aConstr = \d c   -> constrShow d c
      }

----------------------------------------------------------------
-- Annotation algebra
----------------------------------------------------------------

type Depth = Int

data AnnAlg a = 
  AnnAlg {
    aVar    :: Depth -> Int -> a,         
    aReg    :: Depth -> RegionVar -> a,         
    aLam    :: Depth -> Sort -> a -> a,
    aApl    :: Depth -> a -> a -> a,
    aConstr :: Depth -> Constr -> a,    
    aUnit   :: Depth -> a,
    aTuple  :: Depth -> [a] -> a,
    aProj   :: Depth -> Int -> a -> a,
    aAdd    :: Depth -> a -> a -> a,
    aMinus  :: Depth -> a -> RegionVar -> a,
    aJoin   :: Depth -> a -> a -> a,
    aQuant  :: Depth -> a -> a,
    aInstn  :: Depth -> a -> Type -> a,
    aTop    :: Depth -> Sort -> Constr -> a,
    aBot    :: Depth -> Sort -> a,
    aFix    :: Depth -> Sort -> [a] -> a
  }

idAnnAlg :: AnnAlg Annotation
idAnnAlg = AnnAlg {
  aVar    = \_ -> AVar   ,
  aReg    = \_ -> AReg   ,
  aLam    = \_ -> ALam   ,
  aApl    = \_ -> AApl   ,
  aConstr = \_ -> AConstr,
  aUnit   = \_ -> AUnit  ,
  aTuple  = \_ -> ATuple ,
  aProj   = \_ -> AProj  ,
  aAdd    = \_ -> AAdd   ,
  aMinus  = \_ -> AMinus ,
  aJoin   = \_ -> AJoin  ,
  aQuant  = \_ -> AQuant ,
  aInstn  = \_ -> AInstn ,
  aTop    = \_ -> ATop   ,
  aBot    = \_ -> ABot   ,
  aFix    = \_ -> AFix   
}

foldAnnAlg :: AnnAlg a -> Annotation -> a
foldAnnAlg = foldAnnAlgN (-1)

foldAnnAlgN :: Int -> AnnAlg a -> Annotation -> a
foldAnnAlgN n alg ann = go n ann
  where go d (AVar   idx) = aVar    alg d idx
        go d (AReg   idx) = aReg    alg d idx
        go d (ALam   s a) = aLam    alg d s $ go (d+1) a
        go d (AApl   a b) = aApl    alg d (go d a) (go d b)
        go d (AUnit     ) = aUnit   alg d 
        go d (ATuple as ) = aTuple  alg d (go d <$> as) 
        go d (AProj  i a) = aProj   alg d i (go d a) 
        go d (AAdd   a b) = aAdd    alg d (go d a) (go d b)
        go d (AMinus a r) = aMinus  alg d (go d a) r
        go d (AJoin  a b) = aJoin   alg d (go d a) (go d b)
        go d (AQuant a  ) = aQuant  alg d $ go (d+1) a 
        go d (AInstn a t) = aInstn  alg d (go d a) t
        go d (ATop   s v) = aTop    alg d s v
        go d (ABot   s  ) = aBot    alg d s
        go d (AFix   s a) = aFix    alg d s (go (d+1) <$> a)
        go d (AConstr  c) = aConstr alg d c

----------------------------------------------------------------
-- De Bruijn reindexing
----------------------------------------------------------------

-- | Re-index the debruin indices of an annotation
annReIndex :: (Depth -> Int -> Int) -- ^ Reindex function (depth in body to idx to idx)
           -> Annotation -> Annotation
annReIndex f = foldAnnAlg reIdxAlg
  where reIdxAlg = idAnnAlg {
    aLam    = \d s a -> ALam (sortReIndex f d s) a,
    aFix    = \d s a -> AFix (sortReIndex f d s) a,
    aConstr = \d c   -> AConstr (constrReIndex f d c), 
    aTop    = \d s c -> ATop (sortReIndex f d s) (constrReIndex f d c), 
    aBot    = \d s   -> ABot (sortReIndex f d s), 
    aVar    = \d idx -> AVar (f d idx)
  }

-- | Increase all unbound variables by the substitution depth
annWeaken :: Depth -- ^ Depth of the substitution
          -> Annotation -> Annotation
annWeaken subD = annReIndex (weakenIdx subD)

-- | Decrease all unbound indexes by 1
annStrengthen :: Annotation -> Annotation
annStrengthen = annReIndex strengthenIdx

----------------------------------------------------------------
-- Annotation utilities
----------------------------------------------------------------

-- | Convert an annotation tuple to a haskell tuple
liftTuple :: Annotation -> (Annotation, Effect)
liftTuple a = (AProj 0 a, AProj 1 a) 

-- | Convert an annotation tuple to a haskell tuple
unliftTuple :: (Annotation, Effect) -> Annotation 
unliftTuple (a,b) = ATuple [a,b] 


-- | Collect all region variables in an annotation
collect :: Bound -> Annotation -> Constr
collect (Nat 0) _     = M.empty
collect _ AUnit       = M.empty
collect _ (ABot    _) = M.empty 
collect n (AVar    a) = M.singleton (AnnVar a) n 
collect n (AReg    a) = M.singleton (Region a) n 
collect n (AProj i a) = M.mapKeys (CnProj i) $ collect n a
collect n (ATuple ps) = foldr constrAdd M.empty $ map (collect n) ps
collect _ _ = rsError "collect: Collect of non region annotation"

-- | Is annotation a constraint set?
isConstr :: Annotation -> Bool
isConstr (AConstr _) = True
isConstr _           = False


-- | Convert a constraint index to an annotation
constrIdxToAnn :: ConstrIdx -> Annotation 
constrIdxToAnn (Region r)   = AReg r
constrIdxToAnn (AnnVar a)   = AVar a
constrIdxToAnn (CnProj i c) = AProj i $ constrIdxToAnn c


-- | Clean local regions from the annotation
annRemLocalRegs :: Annotation -> Annotation
annRemLocalRegs = foldAnnAlg cleanAlg
  where cleanAlg = idAnnAlg {
    aMinus  = \_ a _ -> a,
    aReg    = \_ r   -> if r == RegionGlobal then AReg RegionGlobal else ABot SortMonoRegion,
    aConstr = \_     -> AConstr . constrRemLocalRegs,
    aTop    = \_ s   -> ATop s . constrRemLocalRegs
  }

