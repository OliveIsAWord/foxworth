-- | A collection of miscellaneous useful functions.
module Utils (getAll, readPrecImpl, prettyShow, root, children) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (void)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.Read qualified as TRead

-- | A list of all values of an enumeration in ascending order.
getAll ∷ (Enum a, Bounded a) ⇒ [a]
getAll = [minBound ..]

-- | A 'Read' implementation for 'Show'able enumerations.
readPrecImpl ∷
    (Show a, Enum a, Bounded a) ⇒ TRead.ReadPrec a
readPrecImpl =
    TRead.parens
        . TRead.choice
        . map (\x → p (show x) x)
        $ getAll
  where
    p str con = TRead.lift $ con <$ ReadP.string str

prettyShow ∷
    (Show (t ()), Functor t, Foldable t) ⇒
    Int →
    Cofree t a →
    Text
prettyShow indent e =
    T.stripEnd . T.unlines $
        (T.replicate indent "  " <> (T.pack . show $ root e))
            : map (prettyShow $ indent + 1) (children e)

root ∷ (Functor f) ⇒ Cofree f a → f ()
root (_ :< e) = void e

children ∷ (Foldable f) ⇒ Cofree f a → [Cofree f a]
children (_ :< e) = toList e
