module Parser.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Int (toNumber)
import Math (pi) as Math
import Parser.Error (Expect, parseError)
import Parser.Syntax (Dual, Expr(..), Binop(..), Unop(..), Cmd(..), fromNumber)
import Parser.Token (token)
import Text.Parsing.Parser (ParseError(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)

type P a = Parser String a

parens :: forall a. P a -> P a
parens = token.parens

reservedOp :: String -> P Unit
reservedOp = token.reservedOp

reserved :: String -> P Unit
reserved = token.reserved

identifier :: P String
identifier = token.identifier

dual :: P Dual
dual = fromNumber <$> (try token.float <|> (toNumber <$> token.integer))

pi :: P Dual
pi = reserved "pi" $> fromNumber Math.pi

lit :: P Expr
lit = Lit <$> dual <|> Lit <$> pi

var :: P Expr
var = Var <$> identifier

term :: P Expr -> P Expr
term p = parens p <|> lit <|> var

-- ORDER MATTERS !!!!
table :: OperatorTable Identity String Expr
table =
  [ [ Prefix (reservedOp "-" $> Unop Negate)
    , Prefix (reservedOp "sqrt" $> Unop Sqrt)
    , Prefix (reservedOp "ln" $> Unop Log)
    , Prefix (reservedOp "exp" $> Unop Exp)
    , Prefix (reservedOp "cosh" $> Unop Cosh)
    , Prefix (reservedOp "cos" $> Unop Cos)
    , Prefix (reservedOp "tanh" $> Unop Tanh)
    , Prefix (reservedOp "tan" $> Unop Tan)
    , Prefix (reservedOp "sinh" $> Unop Sinh)
    , Prefix (reservedOp "sin" $> Unop Sin)
    , Prefix (reservedOp "asinh" $> Unop Asinh)
    , Prefix (reservedOp "asin" $> Unop Asin)
    , Prefix (reservedOp "acosh" $> Unop Acosh)
    , Prefix (reservedOp "acos" $> Unop Acos)
    , Prefix (reservedOp "atanh" $> Unop Atanh)
    , Prefix (reservedOp "atan" $> Unop Atan)
    ]
  , [ Infix (reservedOp "^" $> Binop Pow) AssocRight ]
  , [ Infix (reservedOp "*" $> Binop Mul) AssocLeft
    , Infix (reservedOp "/" $> Binop Div) AssocLeft ]
  , [ Infix (reservedOp "+" $> Binop Add) AssocLeft
    , Infix (reservedOp "-" $> Binop Sub) AssocLeft ]
  ]

expr :: P Expr
expr = fix allExprs
  where
    allExprs p = buildExprParser table (term p)

evalExpr :: P Cmd
evalExpr = Eval <$> expr

parse :: String -> Expect Cmd
parse s = case runParser s evalExpr of
  Left (ParseError message pos) -> parseError message
  Right c -> pure c
