-- | Transforming a list of tokens into a parse tree.
module Parse (parse, prettyFoxProgram) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (void)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as Om
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Lex (Token)
import Lex qualified as Tok
import Syntax (Span, Spanned (..), combinedSpans, sad)
import Text.Megaparsec qualified as M
import Text.Show.Deriving (deriveShow1)

data FoxExprF self
    = Var Text
    | App self self
    | Lambda {upper ∷ Bool, var ∷ Text, sort ∷ self, body ∷ self}
    | Forall {var ∷ Text, sort ∷ self, body ∷ self}
    | Arrow self self
    | Star
    | Box
    | Paren self
    | Bracket self
    | Let {var ∷ Text, sort ∷ self, value ∷ self, body ∷ self}
    deriving (Functor, Foldable, Show)

$(deriveShow1 ''FoxExprF)

type FoxExpr = Cofree FoxExprF Span

prettyFoxExpr ∷ Int → FoxExpr → Text
prettyFoxExpr indent e =
    T.stripEnd . T.unlines $
        (T.replicate indent "  " <> (T.pack . show $ meow e))
            : map (prettyFoxExpr $ indent + 1) (woof e)

-- meow :: FoxExpr -> FoxExprF ()
meow ∷ (Functor f) ⇒ Cofree f a → f ()
meow (_ :< e) = void e

-- woof :: FoxExpr -> [FoxExpr]
woof ∷ (Foldable f) ⇒ Cofree f a → [Cofree f a]
woof (_ :< e) = toList e

type FoxExprUnspanned = FoxExprF FoxExpr

newtype FoxProgram = FoxProgram
    { definitions ∷ OMap Text (Span, FoxExpr)
    -- ^ A list of global named constants, each with a span of the name
    }
    deriving (Show)

prettyFoxProgram ∷ FoxProgram → Text
prettyFoxProgram p =
    T.unlines . map (\(name, (_, body)) → name <> " =\n" <> prettyFoxExpr 1 body) $
        Om.assocs p.definitions

-- | Parse the given token list into a syntax tree, returning index of erroneous token on failure.
parse ∷ FilePath → [Spanned Token] → Either Text FoxProgram
parse filePath tokens = case M.parse pProgram filePath tokens of
    -- TODO: This error handling is very bad!
    Left e → Left . T.pack . M.errorBundlePretty $ e
    Right x → Right x

type Parser = M.Parsec Void [Spanned Token]

pProgram ∷ Parser FoxProgram
pProgram = do
    definitions ← Om.fromList <$> M.many pDefinition <* M.eof
    pure $ FoxProgram{..}

pDefinition ∷ Parser (Text, (Span, FoxExpr))
pDefinition = do
    _ ← pToken Tok.Let
    (span :> name) ← pIdent
    _ ← pToken Tok.Equals
    expr ← pExpr
    pure (name, (span, expr))

pExpr ∷ Parser FoxExpr
pExpr = do
    x ← M.choice [pLet, pAbs, pAtom]
    args ← M.many pAtom
    arrows ← M.many $ pToken Tok.Arrow *> pExpr
    let app = foldl (combinedSpans App) x args
    let arrow = foldl (combinedSpans Arrow) app arrows
    pure arrow

pLet ∷ Parser FoxExpr
pLet = do
    spanStart ← pToken Tok.Let
    (_ :> var) ← pIdent
    _ ← pToken Tok.Colon
    sort ← pExpr
    _ ← pToken Tok.Equals
    value ← pExpr
    _ ← pToken Tok.In
    body@(spanEnd :< _) ← pExpr
    pure $ spanStart <> spanEnd :< Let{..}

pAbs ∷ Parser FoxExpr
pAbs = do
    (spanStart :> upper) ←
        M.choice
            [ (:> Just True) <$> pToken Tok.LambdaUpper
            , (:> Just False) <$> pToken Tok.LambdaLower
            , (:> Nothing) <$> pToken Tok.Forall
            ]
    _ :> var ← pIdent
    _ ← pToken Tok.Colon
    sort ← pExpr
    _ ← pToken Tok.Dot
    body@(spanEnd :< _) ← pExpr
    pure . (spanStart <> spanEnd :<) $ case upper of
        Just upper → Lambda{..}
        Nothing → Forall{..}

pAtom ∷ Parser FoxExpr
pAtom =
    M.choice
        [ sad . fmap Var <$> pIdent
        , (:< Star) <$> pToken Tok.Star
        , (:< Box) <$> pToken Tok.Box
        , pEnclosed Tok.ParenOpen Tok.ParenClose Paren
        , pEnclosed Tok.BracketOpen Tok.BracketClose Bracket
        -- , Bracket <$> (pToken Tok.BracketOpen *> pExpr <* pToken Tok.BracketClose)
        ]

pEnclosed ∷ Token → Token → (FoxExpr → FoxExprUnspanned) → Parser FoxExpr
pEnclosed open close constructor = do
    start ← pToken open
    inner ← pExpr
    end ← pToken close
    pure $ start <> end :< constructor inner

{-
-- | Add span based on the tokens consumed by a parser. This is a linear time operation and should be avoided.
pSpan ∷ Parser FoxExprUnspanned → Parser FoxExpr
pSpan p = do
    (tokens, x) ← M.match p
    let
        s1 :> _ = head tokens
        s2 :> _ = last tokens
    pure $ s1 <> s2 :< x
-}

pToken ∷ Token → Parser Span
pToken t = M.try $ do
    full@(span :> t') ← M.anySingle
    if t == t'
        then pure span
        else
            M.failure
                (Just . M.Tokens $ full :| [])
                (S.singleton . M.Tokens $ (span :> t) :| [])

pIdent ∷ Parser (Spanned Text)
pIdent =
    M.token
        (\case (span :> Tok.Identifier name) → Just $ span :> name; _ → Nothing)
        (S.singleton . M.Label $ 'v' :| "ariable")
