-- | Chunking source code into tokens
module Lex (Token (..), tokenize, showPretty) where

{- Note [Lexing architecture]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use Megaparsec, a standard parser combinator library, for our lexing. Some considerations:

- This code feels rather poor and fragile. Would it be easier to hand-write a lexer?

- Eventually, we'd like to continue compilation gracefully after errors, potentially as far as running code that has lexing errors. That may be a bit far, but it would be preferable to at least advance to the parsing stage and get some useful diagnostics there. It's unclear how exactly to handle each error, such as unknown symbols or mismatched brackets (including block comments).

- Performance is likely dubious, especially given that we liberally use the expensive 'getSourcePos'. If this is a concern, it may be better to consider a different approach to storing source locations (See Note [Source location representation]) or a different lexer such as Alex.
-}

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Syntax (Loc (..), Span (..), Spanned (..))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as MLexer

-- | A single unit of program text made of one or more characters.
data Token
    = -- | An alphanumeric identifier, often a variable
      Identifier Text
    | -- | @let@
      Let
    | -- | @in@
      In
    | -- | @forall@ or @∀@
      Forall
    | -- | @->@ or @→@
      Arrow
    | -- | @=@
      Equals
    | -- | @Type@ or @*@
      Star
    | -- | @Box@ or @□@
      Box
    | -- | @.@
      Dot
    | -- | @:@
      Colon
    | -- | @(@
      ParenOpen
    | -- | @)@
      ParenClose
    | -- | @[@
      BracketOpen
    | -- | @]@
      BracketClose
    | -- | @/\@ or @Λ@
      LambdaUpper
    | -- | @\@ or @λ@
      LambdaLower
    deriving (Eq, Ord, Show)

showPretty ∷ Spanned Token → String
showPretty (s :> t) = case t of
    Identifier t → T.unpack t
    Let → "let"
    In → "in"
    Forall → if len == 6 then "forall" else "∀"
    Arrow → if len == 2 then "->" else "→"
    Equals → "="
    Star → if len == 4 then "Star" else "*"
    Box → if len == 3 then "Box" else "□"
    Dot → "."
    Colon → ":"
    ParenOpen → "("
    ParenClose → ")"
    BracketOpen → "["
    BracketClose → "]"
    LambdaUpper → if len == 2 then "/\\" else "Λ"
    LambdaLower → "λ" -- we can't distinguish "\" and "λ"
  where
    len = s.end.offset - s.start.offset

instance M.VisualStream [Spanned Token] where
    showTokens _ = insertSpaces
      where
        insertSpaces (x :| []) = showPretty x
        insertSpaces (x@(span1 :> _) :| (y@(span2 :> _) : ys)) =
            showPretty x
                <> replicate (span2.start.offset - span1.end.offset) ' '
                <> insertSpaces (y :| ys)
    tokensLength _ tokens = a.start.offset - b.end.offset
      where
        (a :> _) = NE.head tokens
        (b :> _) = NE.last tokens

instance M.TraversableStream [Spanned Token] where
    reachOffset o s = (Just string, M.PosState{..})
      where
        string =
            M.showTokens (Proxy @[Spanned Token]) . NE.fromList $
                filter (\(s :> _) → s.start.line == span.start.line) s.pstateInput
        pstateInput = drop o s.pstateInput
        pstateOffset = s.pstateOffset + o
        (span :> _) = head pstateInput
        pstateSourcePos =
            M.SourcePos
                { sourceName = s.pstateSourcePos.sourceName
                , sourceLine = M.mkPos span.start.line
                , sourceColumn = M.mkPos span.start.column
                }
        pstateTabWidth = s.pstateTabWidth
        pstateLinePrefix = s.pstateLinePrefix

-- | Parse the given source code into a list of tokens, returning a pretty error on failure.
tokenize ∷ FilePath → Text → Either Text [Spanned Token]
tokenize filePath text = case M.parse pTokens filePath text of
    Left e → Left . T.pack $ M.errorBundlePretty e
    Right x → Right x

-- | Custom lexing errors.
data LexError
    = -- | An identifier conflicts with a keyword or punctuation that may be added in the future.
      ReservedSymbol Text
    | -- | A @{\-@ without a corresponding @-\}@
      UnmatchedCommentOpen
    | -- | A @-\}@ without a corresponding @{\-@
      UnmatchedCommentClose
    deriving (Eq, Ord, Show)

instance M.ShowErrorComponent LexError where
    showErrorComponent (ReservedSymbol ident) = "The symbol '" <> T.unpack ident <> "' is reserved"
    showErrorComponent UnmatchedCommentOpen = "unclosed comment block"
    showErrorComponent UnmatchedCommentClose = "unmatched comment block close"

    errorComponentLen (ReservedSymbol ident) = T.length ident
    errorComponentLen UnmatchedCommentOpen = 2
    errorComponentLen UnmatchedCommentClose = 2

-- | Convert our custom LexError into a Megaparsec ParseError
liftLexError ∷ Int → LexError → M.ParseError Text LexError
liftLexError o = M.FancyError o . S.singleton . M.ErrorCustom

-- | Replace a non-custom error with an arbitrary custom one
withError ∷ Int → LexError → Parser a → Parser a
withError o e =
    M.region
        ( \case
            -- TODO: This code is awful and probably incorrect
            x@(M.FancyError _ s) | [M.ErrorCustom _] ← S.toList s → x
            _ → liftLexError o e
        )

-- | Parser type that consumes text.
type Parser = M.Parsec LexError Text

-- | Parse a list of tokens, possibly separated by whitespace.
pTokens ∷ Parser [Spanned Token]
pTokens = pSpace *> M.many (pSpan pToken <* pSpace) <* M.eof

-- | Consume as many whitespace characters and comments as possible.
pSpace ∷ Parser ()
pSpace =
    MLexer.space
        M.space1
        (MLexer.skipLineComment "--")
        pBlockComment

-- | Parses a (possibly nested) block comment
pBlockComment ∷ Parser ()
-- TODO: This code errors on the innermost unclosed comment block open. Haskell, for example, points to the outermost. Which behavior is preferable?
pBlockComment = do
    o ← M.getOffset
    _ ←
        M.string "{-"
            *> withError o (ReservedSymbol "{-#") (M.notFollowedBy (M.string "#"))
    let
        restOfBlock =
            void $
                M.manyTill
                    (pBlockComment <|> pCommentBody)
                    (M.string "-}")
        pCommentBody = do
            o ← M.getOffset
            c ← M.anySingle
            Control.Monad.when (c == '#')
                . withError o (ReservedSymbol "#-}")
                . M.notFollowedBy
                . void
                $ M.string "-}"
    withError o UnmatchedCommentOpen restOfBlock

-- | Consumes input and errors on an (unmatched) closing comment block
pCommentClose ∷ Parser Token -- this type should really be 'Parser Void' but this is more convenient
pCommentClose = do
    o ← M.getOffset
    _ ← M.string "-}"
    let rawError = M.customFailure UnmatchedCommentClose
    M.region (M.setErrorOffset o) rawError

-- | Parse a single token
pToken ∷ Parser Token
pToken =
    M.choice
        [ pCommentClose
        , Forall <$ M.string "∀"
        , Arrow <$ M.string "->"
        , Arrow <$ M.string "→"
        , Equals <$ M.string "="
        , Star <$ M.string "*"
        , Box <$ M.string "□"
        , Dot <$ M.string "."
        , Colon <$ M.string ":"
        , ParenOpen <$ M.string "("
        , ParenClose <$ M.string ")"
        , BracketOpen <$ M.string "["
        , BracketClose <$ M.string "]"
        , LambdaUpper <$ M.string "/\\"
        , LambdaUpper <$ M.string "Λ"
        , LambdaLower <$ M.string "\\"
        , LambdaLower <$ M.string "λ"
        , pWord
        ]

-- | Parse a keyword or identifier
pWord ∷ Parser Token
pWord = do
    o ← M.getOffset
    x ← M.letterChar <|> M.char '_'
    xs ← M.many $ M.alphaNumChar <|> M.char '_' <|> M.char '\''
    let ident = T.pack $ x : xs
    let rawError = M.customFailure $ ReservedSymbol ident
    let e = M.region (M.setErrorOffset o) rawError
    case ident of
        "let" → pure Let
        "in" → pure In
        "forall" → pure Forall
        "Type" → pure Star
        "Box" → pure Box
        "match" → e
        "data" → e
        "type" → e
        _ → pure . Identifier $ T.copy ident

-- | Bundle the output of a parser with its span.
pSpan ∷ Parser a → Parser (Spanned a)
pSpan p = do
    start ← getLoc
    x ← p
    end ← getLoc
    pure $ Span{..} :> x

-- | Get the source location at the current point of parsing.
getLoc ∷ (M.TraversableStream s, M.MonadParsec e s m) ⇒ m Loc
getLoc = do
    M.SourcePos{..} ← M.getSourcePos
    offset ← M.getOffset
    pure Loc{offset, line = M.unPos sourceLine, column = M.unPos sourceColumn}
