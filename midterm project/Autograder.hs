module Autograder where

import Text.Printf
import Data.Char
import Data.Map.Strict as Map
import Data.List

-- Midterm types
data Token = LPar | RPar | Literal String | Plus | Minus | Asterisk | Slash deriving (Eq, Show)
data Op = Add | Sub | Mul | Div deriving (Eq, Show)
data Expr = Binary Op Expr Expr | Val Double deriving (Eq, Show)
data RunError = MismatchedParentheses | LexFailure LexError | ParseFailure ParseError deriving (Eq, Show)
newtype LexError = UnknownToken String deriving (Eq, Show)
newtype ParseError = SyntaxError String deriving (Eq, Show)

newtype Solution = Student String deriving Show
data QuestionGrade = Pass String | Fail String String

instance Show QuestionGrade where
  show r = case r of
    Pass n -> "[PASS] " ++ n
    Fail n r -> "[FAIL] " ++ n ++ " (reason: " ++ r ++ ")"

qName :: QuestionGrade -> String
qName (Pass n) = n
qName (Fail n _) = n

points :: QuestionGrade -> Integer
points r = case r of
  Pass _ -> 1
  _ -> 0

class HW a where
  name :: a -> String
  parseSol :: a -> [Token] -> Either ParseError Expr
  lexSol :: a -> String -> Either LexError [Token]
  validParensSol :: a -> String -> Bool
  evalSol :: a -> Expr -> Double
  runSol :: a -> String -> String

data Score = Score Integer Integer

instance Show Score where
  show (Score correct total) = "Total "
    ++ show correct
    ++ "/"
    ++ show total
    ++ " ("
    ++ printf "%.2f" ((fromIntegral correct :: Double) / (fromIntegral total :: Double) * 100)
    ++ "%)"

parse :: [String] -> [Int]
parse [] = []
parse (x:xs) = if x == "-e" then loop xs [] else []
  where loop [] acc = acc
        loop (x:xs) acc = loop xs (acc ++ [read x])

autograde :: HW a => a -> [Int] -> IO ()
autograde s exclusions = do
  let t1 = [LPar, Plus, Literal "1", Literal "2", RPar]
  let e1 = Binary Add (Val 1) (Val 2)
  putStrLn $ "\nExclusions: " ++ show exclusions
  putStrLn $ "Student: " ++ name s
  let manualResults = [ check "parse[1/?]" (Right e1) $ parseSol s t1
                      , check "lex[1/?]" (Right [LPar, Plus, Literal "1", Literal "2", RPar]) $ lexSol s "(+ 1 2)"
                      , check "validParens[1/3]" True $ validParensSol s "(+ 1 2)"
                      , check "eval[1/3]" 3.0 $ evalSol s e1
                      , check "run[1/5]" "3.0" $ runSol s "(+ 1 2)"
                      ]

  let testCases = Prelude.filter (\(i, _) -> i `notElem` exclusions) $ zip [0..] manualResults
  let excludedCases =
        Prelude.map (\(_, x) -> (-1, Fail (qName x) "EXCLUDED"))
        $ Prelude.filter (\(i, _) -> i `elem` exclusions)
        $ zip [0..] manualResults

  let sortedResults = sortBy (\(i, _) (j, _) -> compare i j) testCases ++ excludedCases
  let out = Prelude.concatMap (\(i, x) -> "    " ++ show x ++ "\n") sortedResults
  putStrLn out
  let correct = sum $ Prelude.map points manualResults
  let score = Score correct (toInteger $ length manualResults + length exclusions)
  putStrLn $ "  " ++ show score



check :: (Eq a, Show a) => String -> a -> a -> QuestionGrade
check name expected actual = if expected == actual
                             then Pass name
                             else Fail name ("Expected: "
                                        ++ show expected
                                        ++ " Got: "
                                        ++ show actual)
