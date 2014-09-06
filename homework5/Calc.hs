{-# LANGUAGE FlexibleInstances #-}
module Calc where

import qualified ExprT as E
import Parser
import qualified StackVM as VM
import qualified Data.Map as M
import Control.Monad

-- exercise 1 : evaluating an expression

eval :: E.ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add a b) = eval a + eval b
eval (E.Mul a b) = eval a * eval b

-- exercise 2 : evaluating a string

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

-- exercise 3 : a type class for expressions

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id

-- exercise 4 : instances of the Expr type class

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = lit $ max a b
  mul (MinMax a) (MinMax b) = lit $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- exercise 5: a stack-based VM

instance Expr VM.Program where
  lit n = [VM.PushI n]
  add p q = p ++ q ++ [VM.Add]
  mul p q = p ++ q ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

testProgram = testExp :: Maybe VM.Program

-- exercise 6: storing and retrieving values

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  add a b m = liftM2 (+) (a m) (b m)
  mul a b m = liftM2 (*) (a m) (b m)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
