{- CS 152 Homework 6
   Team Name: Smart Spartans -}

import  Data.Char

data Token = Operator Char
           | OpenParen
           | CloseParen
           | Number Float
    deriving (Show, Eq)

data ParseTree = NumNode Float
               | OpNode Char [ParseTree]
          deriving Show

-- from HW 5
takeRest :: (a -> Bool) -> [a] -> [a]
takeRest _ [] = []
takeRest p (x : xs)
    | p x = takeRest p xs
    | otherwise = x : xs

isNumberOrDot :: Char -> Bool
isNumberOrDot x
    | x == '.' = True
    | isDigit x = True
    | otherwise = False

takeFirst :: (a -> Bool) -> [a] -> [a]
takeFirst _ [] = []
takeFirst p (x : xs)
    | p x = x:takeFirst p xs
    | otherwise = []

-- STEP 1
scan ([]) = [] -- base case
scan (c : cs)
    | elem c "+" = Operator c: scan cs
    | elem c "*" = Operator c: scan cs
    | elem c "-" = Operator c: scan cs
    | elem c "/" = Operator c: scan cs
    | isNumberOrDot c = Number (read (takeFirst isNumberOrDot (c:cs))): scan (takeRest isNumberOrDot cs)
    | c == '(' = OpenParen : scan cs
    | c == ')' = CloseParen : scan cs
    | isSpace c = scan cs
    | otherwise = error $ "Illegal Character: " ++ [c]

-- STEP 2: uncomment each step as you work on it

--recognize (with any argument) should call expr then look at the return value.
--If the return value is (True, []) then recognize should return True.
--Otherwise, it should return False.
recognize :: [Token] -> Bool
recognize ts
          | expr ts == (True, []) = True
          | otherwise = False


{-
<operands> ->  <expr>[<operands>]

The operands grammar states that when the <expr> appears on the right hand
side in the <operands> rule, the expr function is called.

Then, when <operand> appears on the right hand side in the <expr> or <operands>
rules, you must call the operands function.

Below is our implementation of this grammar rule.
-}

operands :: [Token] -> (Bool, [Token])
operands [] = error " "
operands ts = let (exp, r1) = expr ts -- always have a call to expr followed by a recursive call to operands.
                  in case r1 of -- if r1 starts with a Number or an OpenParen, then you call operands
                  OpenParen: xs -> operands r1
                  Number x: xs -> operands r1
                  _ -> (exp, r1)  -- Otherwise you're done:  you just return exp and r1.
operands_ = error $ "unexpected" ++ [] -- returns incorrect grammar


{-
<expr> -> OPENPAREN OPERATOR <operands> CLOSEPAREN | NUMBER

The expr grammar states that we have to start with open parenthesis followed by an operator,
then we can call operands.

We look at the return value for operands, the return value returns True for all the tokens
except closed parenthesis.

However, if the rest of the tokens returned by operands starts with a closed parenthesis,
then this is a correct expression.
-}

expr :: [Token] -> (Bool, [Token])
expr [] = (False, []) -- ok
expr (OpenParen: (Operator c): rest) = let (op, r1) = operands rest
              in case r1 of
              CloseParen: xs -> (True, xs)
              _-> error "Close Paren expected"
expr (Number x: rest) = operands rest
expr rest = (False, [])
expr_ = error "Expression Expected"

check:: String -> Bool
check = recognize.scan

{-
STEP 3: uncomment each step as you work on it
first call <expr> and then look at the next remaining tokens(using let
expression to examine what the tokens) ; if you have an open paren or number,
then call poperand function recursively; if there's no tokens, then just this <expr> as a
list and return it
ex: if the first element in the rest of tokens is an open paren or num, then
cons the <expr> to operands
CANNOT HAVE AN OPERATOR AFTER OPERAND, OPERATOR CAN ONLY APPEAR AFT


build :: [Token] -> ParseTree
build toks = let (tree, rest) = pexpr toks
             in
               if null rest
               then tree
               else error "Incorrect token"

{-
<poperands> ->  <pexpr>[<poperands>]

The poperands is the same grammar as operands, but instead it retruns a list of parse trees and not a boolean.
-}
poperands :: [Token] -> ([ParseTree], [Token])
poperands ts = let (pexp, r1) = pexpr ts -- always have a call to pexpr followed by a recursive call to operands.
                  in case r1 of -- if r1 starts with a Number or an OpenParen, then you call poperands
                  OpenParen:[] -> poperands ts
                  Number x:[] -> poperands ts
                  otherwise -> pexp && r1 -- Otherwise you're done:  you just return exp and r1.
poperands_ = error "Parse-Operands expected"

{-
<pexpr> -> OPENPAREN OPERATOR <poperands> CLOSEPAREN | NUMBER

The pexpr is the same grammar as expr, but instead it retruns a parse tree of OpNodes and NumNodes
-}
pexpr :: [Token] -> (ParseTree, [Token])
pexpr [] = ([] , []) -- ok
pexpr (OpenParen: (Operator c): rest) = poperands rest
pexpr rest = let (op, r1) = poperands rest
              in case r1 of
              CloseParen:[] -> ([] , [])
              Number x:[] -> (NumNode x, rest)
              Operator x:[] -> (OpNode x, rest)
pexpr_ = error "Parse-Expression expected"

parse:: String -> ParseTree
parse = build.scan


-- STEP 4: uncomment each step as you work on it

{-
We evaluate the function here.

We first start with evaluating everything in the parentheses
within the overall parentheses

Then we go step by step until we are left with only
one OpenParen and one CloseParen.

(+ 4 (* 3 (- 5 2)) 0.1 5.6 (- 5.3 1 2) (/ 10 2))
(+ 4 (* 3 3) 0.1 5.6 (2.3) (5))
(+ 4 (9) 0.1 5.6 (2.3) (5))
(26)

We should use map and fold.

Map goes through a list of size n and returns a list of the same size.
Fold goes through a list of size n and returns an element.

We will use map and fold by using map to go through each element and using
fold to return the float.
-}

eval :: ParseTree -> Float 0--00
eval (NumNode x) = x
eval (OpNode c (first:[]))
  | elem c "+-" = (operatorf c) 0 (eval first)
  | otherwise = (operatorf c) 1 (eval first)
eval (OpNode c operands) = ? --use foldl, map, and eval

interpret :: String -> Float
interpret = eval.build.scan
-}
