module Helium.Optimization.Pretty where

import Prelude hiding ((<$>))

import Data.Maybe(isJust, fromJust)
import Data.Set(Set)
import qualified Data.Set as Set

import Helium.Optimization.LVM_Syntax
import Helium.Optimization.Types hiding (empty)

import Lvm.Common.Byte(stringFromBytes)
import Lvm.Common.Id(stringFromId)

import Text.PrettyPrint.Leijen

{-
Pretty print:
combinators:
<+> Horizontal concat
</> Horizontal concat preferred
<$> Vertical concat

literals:
empty
char 'c'
text "cs"


-}

{-Module-}
instance Pretty OptimizeModule where
    pretty (OptimizeModule_Module name _ _ decls) =
        text "module" <+> text (stringFromId name) <+> text "where"
            <$> vcat (map pretty $ order decls)
        where order = id -- TODO: Decide on the order of the declarations

{-Decl-}
instance Pretty Decl where
    pretty (Decl_Start name _ _ expr) =
        text (stringFromId name) <+> text "=" <+> pretty expr
    pretty (Decl_Function name _ _ expr _ tpScheme) =
        text (stringFromId name) <+> text "::" <+> text (show tpScheme)
            <$> text (stringFromId name) <+> text "=" <+> pretty expr
    pretty (Decl_Abstract name _ _ _ tpScheme) =
        text (stringFromId name) <+> text "::" <+> text (show tpScheme)
    pretty (Decl_Constructor name _ _ _ _ datalink tpScheme) =
        text "Constructor:" <+> text (stringFromId name)
            <+> text "Data:" <+> text (stringFromId datalink)
    pretty (Decl_Data name _ _ _ typeKind) =
        text "Data:" <+> text (stringFromId name)
            <+> text "TypeKind:" <+> int typeKind
    pretty (Decl_Synonym name _ _ _ _) =
        text "Synonym:" <+> text (stringFromId name)
    pretty (Decl_Custom _ _ _ _) =
        empty

{-Expr-}
instance Pretty Expr where
    pretty (Expr_Let binds expr t) =
        align (pretty binds
            <$> text "in" <+> pretty expr)
    pretty (Expr_Match name alts t) =
        align (pt t (text "match" <+> text (stringFromId name) <+> text "with")
            <$> indent 4 (pretty alts))
    pretty (Expr_Ap expr1 expr2 t) =
        pq expr1 <+> pq expr2
        where
        pq e = let p = pretty e in if isAp e then parens p else p
        isAp (Expr_Ap _ _ _) = True
        isAp _ = False
    pretty (Expr_Lam name expr t) =
        char '\\' <> text (stringFromId name) <+> text "->"
            </> pretty expr
    pretty (Expr_ConId name t) =
        text (stringFromId name)
    pretty (Expr_ConTag tag arity t) =
        parens (char '@' <> pretty tag <> comma <> pretty arity)
    pretty (Expr_Var name t) =
        text (stringFromId name)
    pretty (Expr_Lit lit t) =
        pretty lit

pt t = if isJust t then (<+> nest 4 (parens (text "::" <+> pretty (fromJust t)))) else (<> empty)

{-Binds-}
instance Pretty Binds where
    pretty (Binds_NonRec bind) =
        nest 4 (text "let" <+> pretty bind)
    pretty (Binds_Strict bind) =
        nest 5 (text "let!" <+> pretty bind)
    pretty (Binds_Rec binds) =
        nest 4 (text "let" <+> vcat (map pretty binds))

{-Bind-} -- TODO: future print type here
instance Pretty Bind where
    pretty (Bind_Bind name expr ts) =
        pts $ nest 4 (text (stringFromId name) <+> text "="
            </> pretty expr)
        where
        pts = if isJust ts then (text (stringFromId name) <+> text "::" <+> pretty (fromJust ts) <$>) else (empty <>)

{-Alts-}
instance Pretty Alt where
    pretty (Alt_Alt pat expr) =
        nest 4 (pretty pat <+> text "->"
            </> pretty expr)

{-Pat-}
instance Pretty Pat where
    pretty (Pat_ConId name ids) =
        hsep (map (text . stringFromId) (name:ids))
    pretty (Pat_ConTag tag arity ids) =
        hsep ((parens (char '@' <> pretty tag <> comma <> pretty arity)) :
            (map (text . stringFromId) ids))
    pretty (Pat_Lit lit) =
        pretty lit
    pretty (Pat_Default) =
        text "_"

{-Literal-}
instance Pretty Literal where
    pretty (Literal_LitInt int_) =
        pretty int_
    pretty (Literal_LitDouble double_) =
        pretty double_
    pretty (Literal_LitBytes bytes_) =
        text (show (stringFromBytes bytes_))

{-Types-}
instance Pretty T where
    pretty (TAp (TAp (TCon "=>") t1) t2) = parens (pretty t1) <+> text "=>" <+> pqTap t2
    pretty (TAp (TAp (TCon "->") t1) t2) = nest 4 (pqTap t1 <+> text "->" </> pqTap t2) -- make breaks in type possible
    pretty (TAp (TCon "[]") t) = char '[' <> pretty t <> char ']'
    pretty (TAp p@(TPred _ _) t) = pretty p <> char ',' <+> pretty t
    pretty (TAp t1 t2) = pqTap t1 <+> pqTap t2
    pretty (TCon s) = text s
    pretty (TVar i) = pretty i
    pretty (TPred s t) = parens (pretty s <+> pretty t)
    pretty (TAnn ann t) = pqTap t <> char '^' <> text (show ann)

pqTap t = let p = pretty t in if isTAp t then parens p else p
    where
    isTAp (TAp _ _) = True
    isTAp _ = False

instance Pretty Ts where
    pretty (TsVar x) = text "forall(" <> pretty x <> char ')'
    pretty (Ts vars ct t) = nest 4 (pforall </> pct </> pretty t)
        where
        pforall = text "forall{" <> (if not $ Set.null vars then hcat (map pretty (Set.toList vars)) else empty) <> text "}."
        pct = text "C" <> (if not $ null ct then char '{' <> align (vcat (map pretty ct)) <> char '}' else empty) <> char '.'
    pretty (TsAnn ann ts) = char '(' <> pretty ts <> text ")^" <> text (show ann)

{-Constraints-}
instance Pretty Constraint where
    pretty (EqT d t1 t2) = pretty t1 <+> text "==" <+> pretty t2                            -- t1   == t2
    pretty (EqTs d ts1 ts2) = pretty ts1 <+> text "==" <+> pretty ts2                         -- ts1   == ts2
    pretty (EqInst d t ts) = pretty t <+> text "==" <+> text "Inst" <+> pretty ts                        -- t1 == Inst(ts2)
    pretty (EqGen d ts (t, ct, env)) = pretty ts <+> text "==" <+> text "Gen" <> parens (pretty t <> char ',' <+> pretty ct <> char ',' <+> text "{...env...}")    -- ts1 == Gen(t2,ct,env)
    pretty (EqAnn d ann1 ann2) = text "EqAnn"                     -- phi1 == phi2
    pretty (EqPlus d ann1 ann2 ann3) = text "EqPlusAnn"              -- phi1 == phi2 (+) phi3
    pretty (EqUnion d ann1 ann2 ann3) = text "EqUnionAnn"              -- phi1 == phi2 (U) phi3
    pretty (EqTimes d ann1 ann2 ann3) = text "EqTimesAnn"              -- phi1 == phi2 (*) phi3
    pretty (EqCond d ann1 ann2 ann3) = text "EqCondAnn"              -- phi1 == phi2 |> phi3
