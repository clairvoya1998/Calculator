module Expr where

import Parsing

type Name = String

-- At first, 'Expr' contains only addition and values. You will need to
-- add other operations, and variables
data Expr = Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Power Expr Expr
          | Mod Expr Expr
          | Abs Expr
          | Val Int
          | Var String
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Print Int
             | Eval Expr
  deriving Show

eval :: [(Name, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Int -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = addInt (eval vars x) (eval vars y) -- Adds two numbers together
eval vars (Subtract x y) = subtractInt (eval vars x) (eval vars y) -- Adds the second number from the first
eval vars (Multiply x y) = multiplyInt (eval vars x) (eval vars y) -- Multiplies two numbers together
eval vars (Divide x y) = divideInt (eval vars x) (eval vars y) -- Divides the first number by the second
eval vars (Power x y) = powerInt (eval vars x) (eval vars y) -- power of a integer
eval vars (Mod x y) = modInt (eval vars x) (eval vars y) --mod of divition
eval vars (Abs x) = absInt (eval vars x)
eval vars (Var x) = getVar x vars

-- Add two maybe integers
addInt :: Maybe Int -> Maybe Int -> Maybe Int
addInt (Just x) (Just y) = Just (x + y)
addInt _ _ = Nothing

-- Subtract two maybe integers
subtractInt :: Maybe Int -> Maybe Int -> Maybe Int
subtractInt (Just x) (Just y) = Just (x - y)
subtractInt _ _ = Nothing

-- Multiply two maybe integers
multiplyInt :: Maybe Int -> Maybe Int -> Maybe Int
multiplyInt (Just x) (Just y) = Just (x * y)
multiplyInt _ _ = Nothing

-- Divide two maybe integers
divideInt :: Maybe Int -> Maybe Int -> Maybe Int
divideInt (Just x) (Just y) = Just (x `div` y)
divideInt _ _ = Nothing

--Power up
powerInt :: Maybe Int -> Maybe Int -> Maybe Int
powerInt (Just x) (Just y) = Just (x ^ y)
powerInt _ _ = Nothing

modInt :: Maybe Int -> Maybe Int -> Maybe Int
modInt (Just x) (Just y) = Just (x `mod` y)
modInt _ _ = Nothing

-- absolute values
absInt :: Maybe Int -> Maybe Int
absInt (Just x) = if (x) < 0 then Just (-x) else if (x) >= 0 then Just (x) else Just (x)
absInt _ = Nothing

-- Gets a variable value from its name
getVar :: Name -> [(Name, Int)] -> Maybe Int
getVar name variables = if length results == 1 then Just (snd (head results)) else Nothing where results = filter (\x -> fst x == name) variables

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

charToString :: Char -> String
charToString x = [x]

confirmInt :: Maybe Int -> Int
confirmInt (Just x) = x
confirmInt Nothing = 0


pCommand :: Parser Command
pCommand = do t <- ident
              char '='
              e <- pExpr
              return (Set t e)
            ||| do t <- char '!'
                   i <- int
                   return (Print i)
                 ||| do e <- pExpr
                        return (Eval e)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Subtract t e)
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- int
             return (Val d)
           ||| do char '('
                  e <- pExpr
                  char ')'
                  return e
                 ||| do v <- ident
                        return (Var v)
                      ||| do char '|'
                             e <- pExpr
                             char '|'
                             return (Abs e)
pTerm :: Parser Expr
pTerm = do f <- pPower
           do char '*'
              t <- pTerm
              return (Multiply f t)
            ||| do char '/'
                   t <- pTerm
                   return (Divide f t)
                 ||| return f

pPower :: Parser Expr
pPower = do h <- pFactor
            do char '^'
               f <- pPower
               return (Power h f)
             ||| do char '%'
                    f <- pPower
                    return (Mod h f)
                  ||| return h
