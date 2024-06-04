-- | A collection of miscellaneous useful functions.
module Utils (getAll, readPrecImpl) where

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
