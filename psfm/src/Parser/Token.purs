module Parser.Token where

import Control.Alt ((<|>))

import Text.Parsing.Parser.Token ( LanguageDef, GenLanguageDef(..), TokenParser
                                 , makeTokenParser, letter, alphaNum)
import Text.Parsing.Parser.String (char, oneOf)

languageDef :: LanguageDef
languageDef = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: ""
  , nestedComments: false
  , identStart: letter
  , identLetter: alphaNum <|> char '\''
  , opStart: oneOf ['-', '+', '*', '/', '^']
  , opLetter: oneOf []
  , reservedNames: ["pi"]
  , reservedOpNames: ["+", "-", "*", "/", "^"
                     , "sqrt" , "ln", "exp", "sin", "cos", "tan"
                     , "sinh", "cosh", "tanh", "asin", "acos", "atan"
                     , "asinh", "acosh", "atanh"]
  , caseSensitive: true
}

token :: TokenParser
token = makeTokenParser languageDef
