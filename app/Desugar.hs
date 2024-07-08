module Desugar (ExprF (..), Expr, desugar, DesugarData (..), DesugaredProgram, DesugaredExpr) where

import Control.Comonad.Cofree (Cofree (..), unwrap)
import Parse (FoxExpr, Program)
import Parse qualified as P
import Syntax (Ident, emptyIdent)

{- Note: [Core Expression Datatype]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What a dirty secret! Our 'Expr' type actually bears little resemblance to the grammar of System Fω. Instead, we use Henk [1], an intermediate language based on the lambda cube. This means 'Expr' represents terms, types, kinds, and the super-kind which we call "Box". Using the lambda cube also means dependent types can easily be represented, though it is easy to restrict them in sortchecking.

By our own convention, we use "sort" where one would usually use "type". Terms have sort type, types have sort kind, and kinds have sort Box (Box has no sort and so can't be "sorted"). Instead of typechecking, we do "sortchecking".

'Expr' admits two constructors unfamiliar to System Fω. 'Box' is the sort of kinds (*, * -> *, etc.) in the same way that 'Star' is the sort of types (α, α -> α, etc.). The second, 'Qualification', is the dependent product, which generalizes over function and forall sorts. In [1], it is spelled @Πvar: sort. body@, where the variable @var@ may appear in the expression @body@. In Idris, it is spelled @(var : sort) -> body@. We will spell it @(var: sort) -> body@. A term-level and type-level function sort @A -> B@ can be desugared into @(_: sort) -> body@ (where @_@ does not appear in @body@). A forall quantifier @∀a: *. A@ can be desugared into @(a: *) -> A@.

As a benefit of Henk, the same code that prints/sortchecks/substitutes/etc. terms can also operate on types, reducing code duplication. Using Henk also teases the dangerous, alluring possibility of a dependent typing extension, yielding the full power of the calculus of constructions.

[1] SL Peyton Jones, E Meijer [May 1997] "Henk: a typed intermediate language," Types in Compilation (https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/henk.pdf)
-}

-- | The core expression type. Every Foxworth program (without extensions) can be reduced to this.
data ExprF self
    = -- | The sort of types.
      Star
    | -- | The sort of kinds.
      Box
    | -- | A variable.
      Var Ident
    | -- | A function application.
      self :$ self
    | -- | A function abstraction.
      Lambda {var ∷ Ident, sort ∷ self, body ∷ self}
    | -- | A quantification. See [Core Expression Datatype].
      Pi {var ∷ Ident, sort ∷ self, body ∷ self}
    deriving (Functor, Foldable, Show)

type Expr = Cofree ExprF

type DesugaredExpr = Expr DesugarData

type DesugaredProgram = Program DesugaredExpr

infixl 8 :$

newtype DesugarData = DesugarData
    { originalExpr ∷ FoxExpr
    }
    deriving (Show)

desugar ∷ FoxExpr → DesugaredExpr
desugar originalExpr@(_ :< e) = d $ case e of
    P.Var v → Var v
    P.App a b → desugar a :$ desugar b
    P.Lambda{..} → Lambda{var, sort = desugar sort, body = desugar body}
    P.Forall{..} → Pi{var, sort = desugar sort, body = desugar body}
    P.Arrow a b → Pi{var = emptyIdent, sort = desugar a, body = desugar b}
    P.Star → Star
    P.Box → Box
    P.Paren x → unwrap $ desugar x
    P.Bracket x → unwrap $ desugar x
    P.Let{..} → d Lambda{var, sort = desugar sort, body = desugar body} :$ desugar value
  where
    d = (DesugarData{..} :<)
