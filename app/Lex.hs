-- | Chunking source code into tokens
module Lex (Token (..), tokenize) where

{- Note [Lexing architecture]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use Megaparsec, a standard parser combinator library, for our lexing. Some considerations:

- This code feels rather poor and fragile. Would it be easier to hand-write a lexer?

- Eventually, we'd like to continue compilation gracefully after errors, potentially as far as running code that has lexing errors. That may be a bit far, but it would be preferable to at least advance to the parsing stage and get some useful diagnostics there. It's unclear how exactly to handle each error, such as unknown symbols or mismatched brackets (including block comments).

- Performance is likely dubious, especially given that we liberally use the expensive 'getSourcePos'. If this is a concern, it may be better to consider a different approach to storing source locations (See Note [Source location representation]) or a different lexer such as Alex.
-}

import Control.Applicative ((<|>))
import Control.Monad (void)
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
    | -- | @*@
      Star
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
            if c == '#'
                then withError o (ReservedSymbol "#-}") . M.notFollowedBy . void $ M.string "-}"
                else pure ()
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
        , Let <$ M.string "let"
        , In <$ M.string "in"
        , Forall <$ M.string "forall"
        , Forall <$ M.string "∀"
        , Arrow <$ M.string "->"
        , Arrow <$ M.string "→"
        , Equals <$ M.string "="
        , Star <$ M.string "*"
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
        , Identifier <$> pIdent
        ]

-- | Parse an identifier
pIdent ∷ Parser Text
pIdent = do
    o ← M.getOffset
    x ← M.letterChar <|> M.char '_'
    xs ← M.many $ M.alphaNumChar <|> M.char '_' <|> M.char '\''
    let ident = T.pack $ x : xs
    if ident `elem` ["match", "data", "type", "Type"]
        then do
            let rawError = M.customFailure $ ReservedSymbol ident
            M.region (M.setErrorOffset o) rawError
        else pure ident

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
