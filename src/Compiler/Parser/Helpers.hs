module Compiler.Parser.Helpers where

import Control.Monad (void)
import Data.List.NonEmpty qualified as NE
import Data.Proxy
import Data.Semigroup
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Text.Megaparsec hiding (State, parse, satisfy, token)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char.Lexer qualified as M

-- import Text.Megaparsec.Debug qualified as D

import Compiler.Parser.Lexer
import Compiler.Parser.Lexer qualified as Lexer
import Compiler.Types.SrcLoc

type Parser = Parsec Void TokStream

data TokStream = TokStream
  { lexemes :: [Lexeme]
  , source :: String
  }
  deriving (Eq, Ord)

instance Show TokStream where
  show TokStream{lexemes} = show lexemes

instance Stream TokStream where
  type Token TokStream = Lexeme
  type Tokens TokStream = [Lexeme]

  tokensToChunk Proxy lexemes = lexemes
  chunkToTokens Proxy lexemes = lexemes
  chunkLength Proxy lexemes = length lexemes
  chunkEmpty Proxy lexemes = null lexemes
  take1_ s@TokStream{lexemes} = case lexemes of
    [] -> Nothing
    (l : ls) -> Just (l, s{lexemes = ls})
  takeN_ n s@TokStream{lexemes}
    | n <= 0 = Just ([], s)
    | null lexemes = Nothing
    | otherwise =
        let (xs, ls) = splitAt n lexemes
         in Just (xs, s{lexemes = ls})
  takeWhile_ p s@TokStream{lexemes} =
    let (xs, ls) = span p lexemes
     in (xs, s{lexemes = ls})

instance VisualStream TokStream where
  showTokens _proxy tokenStream =
    sconcat $
      NE.map
        ( \tok ->
            renderString $ layoutPretty defaultLayoutOptions $ prettyTok $ unLoc tok
        )
        tokenStream

instance TraversableStream TokStream where
  reachOffset o pst@PosState{..} =
    let rest = drop (o - pstateOffset) (lexemes pstateInput)
        nextPos = case rest of
          [] -> pstateSourcePos
          (Located _ TokIndent : x@(Located s _) : _) ->
            mkSrcPos $ srcSpanStart s
          (x : _) -> mkSrcPos $ srcSpanStart $ getLoc x
        currentLine = unPos (sourceLine nextPos) - 1
        cLineSrc = case lines (source pstateInput) of
          [] -> Nothing
          ls -> case ls !! currentLine of
            "" -> Nothing
            s -> Just s
     in (cLineSrc, pst{pstateInput = TokStream rest (source pstateInput)})

mkSrcPos :: SrcLoc -> SourcePos
mkSrcPos srcLoc =
  SourcePos
    (T.unpack $ unsafeLocFile srcLoc)
    (mkPos $ unsafeLocLine srcLoc)
    (mkPos $ unsafeLocCol srcLoc)

token :: Lexer.Token -> Parser Lexeme
token t = satisfy (== t) <?> showTokenPretty t

satisfy :: (Lexer.Token -> Bool) -> Parser Lexeme
satisfy p = do
  -- D.dbg "analyze" $ do
  -- st <- getParserState
  -- return (stateOffset st, statePosState st)
  try $ M.satisfy (p . unLoc)

oneOf :: [Lexer.Token] -> Parser Lexeme
oneOf ts = satisfy (`elem` ts)

noneOf :: [Lexer.Token] -> Parser Lexeme
noneOf ts = satisfy (`notElem` ts)

anyToken :: Parser Lexeme
anyToken = satisfy (const True)

reserved :: String -> Parser Lexeme
reserved word = satisfy (== reservedIdToTok word)

reservedOp :: String -> Parser Lexeme
reservedOp op = satisfy (== reservedOpToTok op)

parens :: Parser a -> Parser a
parens = between (token TokLParen) (token TokRParen)

braces :: Parser a -> Parser a
braces = between (token TokLBrace) (token TokRBrace)

brackets :: Parser a -> Parser a
brackets = between (token TokLBracket) (token TokRBracket)

backticks :: Parser a -> Parser a
backticks = between (token TokBackquote) (token TokBackquote)

comma :: Parser ()
comma = void $ token TokComma

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p comma

semicolon :: Parser ()
semicolon = void $ token TokSemicolon

expectedChar :: Char -> Parser Char
expectedChar c = do
  x@(Located _ (TokLitChar t)) <- satisfy $
    \case
      TokLitChar _ -> True
      _ -> False
  if T.singleton c == t
    then pure c
    else failure (Just (Tokens $ NE.singleton x)) (Set.singleton $ Tokens $ NE.singleton x)

charLiteral :: Parser Char
charLiteral = do
  (Located _ (TokLitChar t)) <- satisfy $
    \case
      TokLitChar _ -> True
      _ -> False
  let Right r = M.parse (M.charLiteral @Char) "" t
  pure r

expectedString :: Text -> Parser Text
expectedString s = do
  x@(Located _ (TokLitString t)) <- satisfy $
    \case
      TokLitString _ -> True
      _ -> False
  if s == t
    then pure s
    else failure (Just (Tokens $ NE.singleton x)) (Set.singleton $ Tokens $ NE.singleton x)

stringLiteral :: Parser Text
stringLiteral = do
  result <- do
    expectedChar '"'
    manyTill charLiteral (expectedChar '"')
  pure $ T.pack result

string :: Parser Text
string = do
  (Located _ (TokLitString t)) <- satisfy $
    \case
      TokLitString _ -> True
      _ -> False
  pure t

varId :: Parser Text
varId = do
  (Located _ (TokVarId t)) <- satisfy $
    \case
      TokVarId _ -> True
      _ -> False
  pure t

integer :: Parser Integer
integer = do
  (Located _ (TokLitInteger t)) <- satisfy $
    \case
      TokLitInteger _ -> True
      _ -> False
  let Right r = M.parse (M.decimal @Char) "" t
  pure r

-- expectedInteger :: Int -> Parser Int
-- expectedInteger i = do
--   x@(Located _ (TokLitInteger t )) <- satisfy $
--               \case
--                 TokLitInteger _ -> True
--                 _ -> False
--   if i == t
--   then pure t
--   else failure (Just (Tokens $ NE.singleton x)) (Set.singleton $ Tokens $ NE.singleton x )
