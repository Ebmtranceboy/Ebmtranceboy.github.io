module Parser.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Int (toNumber)
import Parser.Error (Expect, parseError)
import Parser.Syntax (class Real, Expr(..), Binop(..), Unop(..), fromNumber, pi)
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

getLit :: forall a. Real a => P a
getLit = fromNumber <$> (try token.float <|> (toNumber <$> token.integer))

getPi :: forall a. Real a => P a
getPi = reserved "pi" $> pi

lit :: forall a. Real a => P (Expr a)
lit = Lit <$> getLit <|> Lit <$> getPi

var :: forall a. P (Expr a)
var = Var <$> identifier

term :: forall a. Real a => P (Expr a) -> P (Expr a)
term p = parens p <|> lit <|> var

-- ORDER MATTERS !!!!
table :: forall a. OperatorTable Identity String (Expr a)
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

expr :: forall a. Real a => P (Expr a)
expr = fix allExprs
  where
    allExprs p = buildExprParser table (term p)

parse :: forall a. Real a => String -> Expect (Expr a)
parse s = case runParser s expr of
  Left (ParseError message pos) -> parseError message
  Right c -> pure c
