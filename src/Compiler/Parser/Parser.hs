module Compiler.Parser.Parser where

import Prelude hiding (lex)

import Data.Vector qualified as Vector
import Prettyprinter ()
import Text.Megaparsec hiding (State, Token, parse, satisfy, token)

import Compiler.Parser.Helpers
import Compiler.Parser.Lexer
import Compiler.Types.AST
import Compiler.Types.Name
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

parseStatements :: Parser (AST CoreName)
parseStatements = do
  stmts <- many parseStatement
  pure (Block (Vector.fromList stmts))

parseStatement :: Parser (AST CoreName)
parseStatement =
  try (parseAssignment <?> "Assignment")
    <|> (parseFunction <?> "Function")
    <|> (parseReturn <?> "Return")

parseReturn :: Parser (AST CoreName)
parseReturn = do
  reserved "return"
  result <- parseExpression
  semicolon
  pure $ Return result

parseAssignment :: Parser (AST CoreName)
parseAssignment = do
  declarationType
  name <- varId
  token TokAssignment
  body <- parseTerm
  semicolon
  pure $ Let name body

parseTerm :: Parser (PlumeExpr CoreName)
parseTerm =
  label "term" $
    choice
      [ parseNumber
      , parens parseExpression
      , Var <$> parseIdentifier
      ]

parseNumber :: Parser (PlumeExpr CoreName)
parseNumber = Lit . LitInt <$> integer

parseExpression :: Parser (PlumeExpr CoreName)
parseExpression = makeExprParser parseTerm operatorTable <?> "Expression"

operatorTable :: [[Operator Parser (PlumeExpr CoreName)]]
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
  ,
    [ binary TokLessThan LessThan
    , binary TokLessThanOrEqual LessThanOrEqual
    , binary TokGreaterThan GreaterThan
    , binary TokGreaterThanOrEqual GreaterThanOrEqual
    ]
  ,
    [ binary TokEqual Equal
    , binary TokNotEqual NotEqual
    ]
  ,
    [ binary TokAnd And
    , binary TokOr Or
    ]
  ]

binary :: Token -> (PlumeExpr CoreName -> PlumeExpr CoreName -> PlumeExpr CoreName) -> Operator Parser (PlumeExpr CoreName)
binary name f = InfixL (f <$ token name)

prefix :: Token -> (PlumeExpr CoreName -> PlumeExpr CoreName) -> Operator Parser (PlumeExpr CoreName)
prefix name f = Prefix (f <$ token name)

parseFunction :: Parser (AST CoreName)
parseFunction = do
  funType <- declarationType
  funId <- parseIdentifier
  parsedParams <- parseParameters
  result <- braces parseStatements
  pure $ Fun funType funId parsedParams result

parseParameters :: Parser (Vector (Pat CoreName))
parseParameters =
  noParameters <|> parameters

noParameters :: Parser (Vector a)
noParameters = label "no parameters" $ do
  token TokLParen
  token TokRParen
  pure mempty

parameters :: Parser (Vector (Pat CoreName))
parameters = label "parameters" $ do
  token TokLParen
  result <- patternVar `sepBy` comma
  token TokRParen
  pure $ Vector.fromList result

declarationType :: Parser (PlumeType CoreName)
declarationType =
  label "function's return type" $ do
    name <- conId
    pure $ VarType name

patternVar :: Parser (Pat CoreName)
patternVar = PatternVar <$> varId

parseIdentifier :: Parser CoreName
parseIdentifier =
  label "identifier" varId
