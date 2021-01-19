{- CS 152 Homework 5
   Team Name: Smart Spartans -}

import  Data.Char

-- Question 1
takeRest :: (a -> Bool) -> [a] -> [a]
takeRest _ [] = []
takeRest p (x : xs)
    | p x = takeRest p xs
    | otherwise = x : xs


-- Question 2 - Uncomment the function type declaration below
shout ::  [String] -> String
shout [] = ""
shout xs = foldr (\a b-> a ++ "!" ++ b) "" xs


-- Type Declaration for question 3
data Token = Operator
           | OpenParen
           | CloseParen
           | Identifier
           | Number
    deriving Show

-- Question 3 - - Uncomment the function type declaration below
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | elem c "+-*/" = Operator : tokenize cs
    | c == '.' = Number : tokenize (takeRest isDigit cs)
    | isDigit c = Number : tokenize (takeRest isDigit cs)
    | c == '(' = OpenParen : tokenize cs
    | c == ')' = CloseParen : tokenize cs
    | isAlpha c = Identifier : tokenize (takeRest isAlpha cs)
    | isSpace c  = tokenize cs
    | otherwise  = error $ "Illegal Character: " ++ [c]
