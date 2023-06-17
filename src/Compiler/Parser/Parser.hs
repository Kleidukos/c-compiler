module Compiler.Parser.Parser where

import Prelude hiding (lex)

import Data.Text (Text)
import Data.Vector qualified as Vector
import Prettyprinter ()
import Text.Megaparsec hiding (State, Token, parse, satisfy, token)

import Compiler.Parser.Helpers
import Compiler.Parser.Lexer
import Compiler.Types.AST
import Control.Monad.Combinators.Expr
import Data.Vector (Vector)

parse
  :: Parser a
  -- ^ Parser for a file
  -> String
  -- ^ Filename
  -> String
  -- ^ Source
  -> Either String a
parse p fname source = do
  lexemes <- lex fname source
  let out = runParser p fname (TokStream lexemes source)
  case out of
    Left errBundle -> Left $ errorBundlePretty errBundle
    Right val -> Right val

testParser :: Parser a -> String -> Either String a
testParser p = parse p ""

parseStatements :: Parser AST
parseStatements = do
  stmts <- many parseStatement
  eof
  pure (Block (Vector.fromList stmts))

parseStatement :: Parser AST
parseStatement =
  parseFunction
    <|> parseReturn

parseReturn :: Parser AST
parseReturn = do
  reserved "return"
  result <- parseExpression
  semicolon
  pure $ Return result

parseTerm :: Parser PlumeExpr
parseTerm =
  label "term" $
    choice
      [ parseNumber
      , parens parseExpression
      ]

parseNumber :: Parser PlumeExpr
parseNumber = Lit . LitInt <$> integer

parseExpression :: Parser PlumeExpr
parseExpression = makeExprParser parseTerm operatorTable <?> "Expression"

operatorTable :: [[Operator Parser PlumeExpr]]
operatorTable =
  [
    [ -- prefix "+" id
      prefix TokMinus Negate
    , prefix TokLogicalNegation LogicalNegation
    , prefix TokTilde BitwiseComplement
    ]
  ,
    [ binary TokMultiplication Multiplication
    , binary TokDivision Division
    ]
  ,
    [ binary TokAddition Addition
    , binary TokMinus Subtraction
    ]
    -- ,
    -- [ binary "==" Equal
    -- , binary "!=" NotEqual
    -- ]
  ]

binary :: Token -> (PlumeExpr -> PlumeExpr -> PlumeExpr) -> Operator Parser PlumeExpr
binary name f = InfixL (f <$ token name)

prefix :: Token -> (PlumeExpr -> PlumeExpr) -> Operator Parser PlumeExpr
prefix name f = Prefix (f <$ token name)

parseFunction :: Parser AST
parseFunction = do
  varId <?> "function type"
  funId <- varId <?> "function name"
  parsedParams <- parseParameters
  result <- braces parseStatement
  pure $ Fun funId parsedParams result

parseParameters :: Parser (Vector Pat)
parseParameters =
  label "parameter" $
    noParameters <|> parameters

noParameters :: Parser (Vector a)
noParameters = do
  token TokLParen
  token TokRParen
  pure mempty

parameters :: Parser (Vector Pat)
parameters = do
  token TokLParen
  result <- patternVar `sepBy` comma
  token TokRParen
  pure $ Vector.fromList result

expectedFunctionType :: Parser Text
expectedFunctionType = varId

patternVar :: Parser Pat
patternVar = PatternVar <$> varId
