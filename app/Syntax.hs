-- | source code.
module Syntax (
    Loc (..),
    Span (..),
    Spanned (..),
    unspan,
    sad,
    combinedSpans,
    Ident (..),
    emptyIdent,
    isEmpty,
) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Exception (assert)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Records (HasField (..))

{- Note [Source location representation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO
-}

-- | A source code location.
data Loc = Loc
    -- TODO: Measure performance implications for strict fields.
    { offset ∷ Int
    -- ^ Absolute character offset
    , line ∷ Int
    -- ^ Line number
    , column ∷ Int
    -- ^ Column number
    }

instance Eq Loc where
    x == y = x.offset == y.offset

instance Ord Loc where
    compare x y = compare x.offset y.offset

instance Show Loc where
    show loc = show loc.line <> ":" <> show loc.column

-- | A range of characters in the program source.
data Span = Span
    { start ∷ Loc
    -- ^ First character of the span
    , end ∷ Loc
    -- ^ One past the last character of the span
    }
    deriving (Eq, Ord)

instance Semigroup Span where
    a <> b =
        assert
            (a.start <= a.end && a.end <= b.start && b.start <= b.end)
            Span{start = a.start, end = b.end}

instance Show Span where
    show span = show span.start <> "-" <> show span.end

infixr 5 :>

-- | A convenient shorthand for a value associated with a span.
data Spanned a = Span :> a
    deriving (Eq, Ord, Show, Functor)

unspan ∷ Spanned a → a
unspan (_ :> x) = x

-- | Converts from the 'Spanned' constructor ':>' to the 'Cofree' constructor ':<'.
sad ∷ Spanned (f (Cofree f Span)) → Cofree f Span
sad (span :> v) = span :< v

combinedSpans ∷
    (Cofree f Span → Cofree f Span → f (Cofree f Span)) →
    Cofree f Span →
    Cofree f Span →
    Cofree f Span
combinedSpans constructor a@(sa :< _) b@(sb :< _) = sa <> sb :< constructor a b

{-
-- | Calculate the location of a character offset given a source text
offsetToLoc source offset = go 1 (offset + 1) (fmap T.length $ T.lines source)
  where
    go line column (len:lens) | len <= column = go (line + 1) (column - len) lens
    go line column _ = Loc {..}
-}

data Ident = Ident
    { name ∷ Text
    , loc ∷ Loc
    }

emptyIdent ∷ Ident
emptyIdent = Ident{name = "", loc = Loc{offset = -1, line = -1, column = -1}}

isEmpty ∷ Ident → Bool
isEmpty i = i.name == ""

instance HasField "id" Ident Int where
    getField = (.loc.offset)

instance Eq Ident where
    a == b = a.id == b.id

instance Ord Ident where
    compare a b = compare a.id b.id

instance Show Ident where
    show i = if isEmpty i then "_" else T.unpack i.name <> "@" <> show i.id
