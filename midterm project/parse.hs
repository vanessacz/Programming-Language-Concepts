module Main where

import Autograder
import Data.Maybe
import Debug.Trace
import System.Environment
import Text.Read

import Data.Char
{-
 - Note: these types are defined in Autograder.hs and should be used in
 - completing the undefined functions below.
 -
 - data Token = LPar | RPar | Literal String | Plus | Minus | Asterisk | Slash deriving (Eq, Show)
 - data Op = Add | Sub | Mul | Div deriving (Eq, Show)
 - data Expr = Binary Op Expr Expr | Val Double deriving (Eq, Show)
 - data RunError = MismatchedParentheses | LexFailure LexError | ParseFailure ParseError deriving (Eq, Show)
 - newtype LexError = UnknownToken String deriving (Eq, Show)
 - newtype ParseError = SyntaxError String deriving (Eq, Show)
 -}

-- TODO: put your name here
studentName = "Vanessa Chen"

instance HW Solution where
  name a = studentName
  parseSol a = Main.parse
  lexSol a = Main.lex
  validParensSol a = validParens
  evalSol a = eval
  runSol a = run

{-
 - run executes the input expression and returns the result as a string if the
 - given expression is valid. Otherwise, run returns a string error message.
 -}
run :: String -> String
run str = 
  case validParens str of
    False -> show False
    True -> case Main.lex str of
      Left lexError -> show lexError
      Right parseable -> case Main.parse parseable of
        Left syntaxError -> show syntaxError
        Right fin -> show $ eval fin


{-
 - evaluates the given expression, which is assumed to be valid.
 -}
eval :: Expr -> Double
eval (Val n)  = n
eval (Binary Add x y) = eval x + eval y
eval (Binary Sub x y) = eval x - eval y
eval (Binary Mul x y) = eval x * eval y
eval (Binary Div x y) = eval x / eval y


-- Returns `True` if the string can parse as a double, `False` otherwise.
-- You may find this useful but are not strictly required to use it.
isDouble :: String -> Bool
isDouble x = isJust (readMaybe x :: Maybe Double)

{-
 - Checks whether the input string contains balanced parentheses.
 -}
validParens :: String -> Bool
validParens [] = True
validParens str = validParens' str []

validParens' [] stack = null stack
validParens' (x:xs) [] = validParens' xs [x]
validParens' (x:xs) (y:ys)  = 
  if isSpace x || x `notElem` "()" then validParens' xs (y:ys)
    else if x == '(' then validParens' xs (x:y:ys)
    else if (x == ')' && y == '(') then validParens' xs ys 
    else False

{-
 - Lexes the input string, returning either a LexError or a list of tokens.
 -}
lex :: String -> Either LexError [Token]
lex [] = Right []
lex (x:xs) = 
  case Main.lex xs of
    Left error -> Left error
    Right tokens -> 
      case lex' x of
        Left wrong -> Left wrong
        Right t -> Right (t ++ tokens)

lex' x 
  | isSpace x = Right []
  | isDigit x = Right [(Literal [x])]
  | x == '(' = Right [LPar]
  | x == head ")" = Right [RPar]
  | x == head "+" = Right [Plus]
  | x == head "-" = Right [Minus]
  | x == head "*" = Right [Asterisk]
  | x == head "/" = Right [Slash]
  | otherwise = Left (UnknownToken [x])

{-
 - Parses the token list and returns either a ParseError or the parsed expression.
 -}
parse :: [Token] -> Either ParseError Expr
parse ts = do
  let s = Prelude.reverse ts
  parse' ts

parse' [] = Right $ Val 0
parse' (x:xs) = case x of
  LPar -> case Main.parse xs of 
    Left wrong -> Left wrong
    Right lpar -> Right lpar
  RPar -> case Main.parse xs of
    Left wrong -> Left wrong 
    Right rpar -> Right rpar
  Plus -> case Main.parse xs of
    Left wrong -> Left wrong 
    Right addExp -> case Main.parse (tail xs) of
        Left wrong -> Left wrong 
        Right addExp2 -> Right (Binary Add addExp addExp2)
  Minus -> case Main.parse xs of
    Left wrong -> Left wrong 
    Right subExp -> case Main.parse (tail xs) of
        Left wrong -> Left wrong 
        Right subExp2 -> Right (Binary Sub subExp subExp2)
  Asterisk -> case Main.parse xs of
    Left wrong -> Left wrong 
    Right mulEx -> case Main.parse (tail xs) of
        Left wrong -> Left wrong 
        Right mulEx2 -> Right (Binary Mul mulEx mulEx2)
  Slash -> case Main.parse xs of
    Left wrong -> Left wrong 
    Right divExp -> case Main.parse (tail xs) of
        Left wrong -> Left wrong 
        Right divExp2 -> Right (Binary Div divExp divExp2)
  Literal x-> case (readMaybe x :: Maybe Double) of
        Just dub -> Right (Val dub)
        Nothing -> Left (SyntaxError (show x))

main = do
  let s = Student studentName
  args <- getArgs
  let exclusions = Autograder.parse args
  autograde s exclusions
