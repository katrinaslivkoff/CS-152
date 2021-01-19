{- CS 152 Homework 7
   Team Name: Smart Spartans -}

import  Data.Char

data Token = Operator Char
           | OpenParen
           | CloseParen
           | Number Float
           | Let
           | Lambda
           | Identifier String
    deriving (Show, Eq)

data ParseTree = NumNode Float
               | OpNode Char [ParseTree]
               | IdentNode String
               | Binding ParseTree ParseTree
               | LetNode ParseTree ParseTree
               | LambdaNode String ParseTree
               | Application ParseTree ParseTree
          deriving Show

-- STEP 1

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

scan :: String -> [Token]
scan ([]) = [] -- base case
scan (c : cs)
    | elem c "+-/*" = Operator c: scan cs
    | isNumberOrDot c = Number (read (takeWhile isNumberOrDot (c:cs))): scan (takeRest isNumberOrDot cs)
    | c == '(' = OpenParen : scan cs
    | c == ')' = CloseParen : scan cs
    | isAlpha c = let name = (takeWhile isAlpha (c:cs))
                      rest = (takeRest isAlpha cs)
                  in case name of
                    "let" -> Let : scan rest
                    "lambda" -> Lambda : scan rest
                    otherwise -> Identifier name: scan rest
    | isSpace c = scan cs
    | otherwise = error $ "Illegal Character: " ++ [c]

-- STEP 2: uncomment each step as you work on it

build :: [Token] -> ParseTree
build toks = let (tree, rest) = expr toks
             in
               if null rest
               then tree
               else error "Incorrect token"

-- <operands> ->  <expr> [<operands>]
operands :: [Token] -> ([ParseTree], [Token])
operands ts = let (e1, r1) = expr ts
                  in case r1 of
                  OpenParen: xs -> let (e2, r2) = operands r1
                                  in(e1:e2, r2)
                  Number x: xs -> let (e3, r3) = operands r1
                                  in(e1:e3, r3)
                  Identifier x: xs -> let (e4, r4) = operands r1
                                      in(e1:e4, r4)
                  _ -> ([e1], r1)
operands_ = error $ "unexpected" ++ []

-- <expr> -> OPENPAREN OPERATOR <operands> CLOSEPAREN
--         | NUMBER
--         | OPENPAREN LET OPENPAREN <binding> CLOSEPAREN <expr> CLOSEPAREN
--         | <application>
--         | IDENTIFIER
expr :: [Token] -> (ParseTree, [Token])
expr (Number x: xs) = (NumNode x, xs)
expr (Identifier y: ys) = (IdentNode y, ys)
expr (OpenParen: (Operator op): rest) = let (e1, r1) = operands rest
              in case r1 of
              CloseParen: r2 -> (OpNode op e1, r2)
              _-> error "Close Paren expected"
expr (OpenParen: Let: OpenParen: rest) = let (e1, r1) = binding rest
              in case r1 of
              CloseParen: r2 -> let (e2, r3) = expr r2
                                in case r3 of
                                CloseParen: r4 -> (LetNode e1 e2, r4)
              _-> error "Close Paren expected"
expr ts = application ts

-- <binding> -> OPENPAREN IDENTIFIER <expr> CLOSEPAREN
binding :: [Token] -> (ParseTree, [Token])
binding (OpenParen: (Identifier id): rest) = let (e1, r1) = expr rest
              in case r1 of
              CloseParen: r2 -> (Binding (IdentNode id) e1, r2)
              _-> error "Close Paren expected"
binding _ = error "Invalid binding"

-- <function> -> OPENPAREN LAMBDA OPENPAREN IDENTIDIER CLOSEPAREN <expr> CLOSEPAREN
function :: [Token] -> (ParseTree, [Token])
function (OpenParen: Lambda: OpenParen: Identifier id: CloseParen: rest) = let (e1, r1) = expr rest
              in case r1 of
              CloseParen: r2 -> (LambdaNode id e1, r2)
              _-> error "Close Paren expected"
function_ = error "Invalid function"

-- <application> ->  OPENPAREN <function> <expression> CLOSEPAREN
application :: [Token] -> (ParseTree, [Token])
application (OpenParen: rest) = let (e1, r1) = function rest
                                    (e2, r2) = expr r1
              in case r2 of
              CloseParen: r3 -> (Application e1 e2, r3)
              _-> error "Close Paren expected"
application_ = error "Invalid application"

parse:: String -> ParseTree
parse = build.scan

-- STEP 3: uncomment each step as you work on it

bind :: [(String, Float)] -> ParseTree -> [(String,Float)]
bind env (Binding (IdentNode name) val) = (name, eval env val): env

eval :: [(String, Float)] -> ParseTree -> Float
eval env (NumNode x) = x
eval env (OpNode c (first:[]))
  | elem c "+-" = (operatorf c) 0 (eval env first)
  | otherwise = (operatorf c) 1 (eval env first)
eval env (OpNode c (first:rest)) = foldl (operatorf c)(eval env first)(map (eval env) (rest))
eval env (IdentNode str) = let xs = lookup str env
                           in case xs of
                           Nothing -> error "Undefined Identifier"
                           Just x -> x
eval env (LetNode p1 p2) = eval (bind env p1) p2
eval env (Application (LambdaNode str p1) p2) = eval ((str, eval env p2): env) p1
eval _ _ = error "Invalid input"

operatorf '+' = (+)
operatorf '-' = (-)
operatorf '*' = (*)
operatorf '/' = (/)

eval0 :: ParseTree -> Float
eval0 tree = eval [] tree

interpret :: String -> Float
interpret = eval0.build.scan
