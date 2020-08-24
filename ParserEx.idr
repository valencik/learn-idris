module ParserEx

import Text.Lexer
import public Text.Parser.Core
import public Text.Parser

%default total

public export
data ExpressionToken = Number Integer
        | Operator String
        | OParen
        | CParen
        | EndInput

export
Show ExpressionToken where
  show (Number x) = "number " ++ show x
  show (Operator x) = "operator " ++ show x
  show OParen = "("
  show CParen = ")"
  show EndInput = "end of input"

export
Show (TokenData ExpressionToken) where
  show (MkToken l c t) = "line=" ++ show l ++ "col=" ++ show c ++ "tok=" ++ show t

export
opsChar : String
opsChar = "+-*"

operator : Lexer
operator = some (oneOf opsChar)

toInt' : String -> Integer
toInt' = cast

expressionTokens : TokenMap ExpressionToken
expressionTokens =
  [(digits, \x => Number (toInt' x)),
  (operator, \x => Operator x),
  (is '(', \x => OParen),
  (is ')', \x => CParen)]

