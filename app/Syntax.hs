-- | source code.
module Syntax (Loc (..), Span (..), Spanned (..)) where

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
    -- ^ Last character of the span
    }
    deriving (Eq, Ord)

instance Show Span where
    show span = show span.start <> "-" <> show span.end

infixr 5 :>

-- | A convenient shorthand for a value associated with a span.
data Spanned a = Span :> a
    deriving (Eq, Ord, Show)
