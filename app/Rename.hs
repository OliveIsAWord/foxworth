module Rename (renameProgram, RenamedExpr, RenamedProgram, Ident) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Map.Ordered (OMap, (|<))
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import Data.Text qualified as T
import Desugar (
    DesugarData (..),
    DesugaredExpr,
    DesugaredProgram,
    Expr,
    ExprF (..),
 )
import Parse (FoxExpr, Program (..))
import Syntax (Ident (..), isEmpty)

-- import Syntax (Loc (..), Span (..), Spanned (..))

-- synonym of DesugarData for type safety; transforming a DesugaredExpr into a RenamedExpr requires visiting and thus performing name resolution on every subexpression
newtype RenameData = RenameData
    { originalExpr ∷ FoxExpr
    }

type RenamedExpr = Expr RenameData

type RenamedProgram = Program RenamedExpr

newtype NameError = NotFound Ident

instance Show NameError where
    show (NotFound i) = "Variable not defined: " <> T.unpack i.name <> " at " <> show i.loc

renameProgram ∷ DesugaredProgram → Either NameError RenamedProgram
renameProgram p =
    fmap (Program . OMap.fromList) . go OMap.empty $
        OMap.assocs p.definitions
  where
    go _ [] = Right []
    go globals ((ident, body) : ds) = case renameExpr globals body of
        Left e → Left e
        Right x → go ((ident.name, ident) |< globals) ds >>= Right . ((ident, x) :)

renameExpr ∷ OMap Text Ident → DesugaredExpr → Either NameError RenamedExpr
renameExpr env (ddata :< e) = fmap (rdata :<) $ case e of
    Star → Right Star
    Box → Right Box
    Var v → maybe (Left $ NotFound v) (Right . Var) $ OMap.lookup v.name env
    a :$ b → liftA2 (:$) (recurse a) (recurse b)
    Lambda{..} → liftA2 (Lambda var) (recurse sort) (recurseWith var body)
    Pi{..} → liftA2 (Pi var) (recurse sort) (recurseWith var body)
  where
    rdata = let DesugarData{..} = ddata in RenameData{..}
    recurse = renameExpr env
    recurseWith var = if isEmpty var then recurse else renameExpr $ (var.name, var) |< env
