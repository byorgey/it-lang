{-# LANGUAGE GADTs #-}

module ItLang where

import qualified Data.Map as M
import Data.Maybe

type Var = String

data Nat where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Eq, Ord)

type Prog = [Stmt]

data Stmt where
  Assign :: Var -> Exp -> Stmt
  Block  :: Prog -> Stmt
  If     :: BExp -> Stmt -> Stmt -> Stmt
  Repeat :: Exp -> Stmt -> Stmt

data Exp where
  Lit   :: Nat -> Exp
  V     :: Var -> Exp
  Plus  :: Exp -> Exp -> Exp
  Minus :: Exp -> Exp -> Exp
  Times :: Exp -> Exp -> Exp

data BExp where
  BLit :: Bool -> BExp
  Eq   :: Exp -> Exp -> BExp
  Lt   :: Exp -> Exp -> BExp
  Not  :: BExp -> BExp
  Or   :: BExp -> BExp -> BExp
  And  :: BExp -> BExp -> BExp

type Mem = M.Map Var Nat

------------------------------------------------------------

add :: Nat -> Nat -> Nat
add Z y     = y
add (S x) y = S (add x y)


evalExp :: Exp -> Mem -> Nat
evalExp (Lit n) _ = n
evalExp (V v) m       = memLookup v m
evalExp (Plus x y) m  = add (evalExp x m) (evalExp y m)
evalExp (Minus x y) m = sub (evalExp x m) (evalExp y m) where
  sub Z y     = y
  sub (S x) y = sub x y
evalExp (Times x y) m = mul (evalExp x m) (evalExp y m) where
  mul Z y     = Z
  mul (S x) y = add y (mul x y)


------------------------------------------------------------

evalBExp :: BExp -> Mem -> Bool
evalBExp = undefined

------------------------------------------------------------

execStmt :: Stmt -> Mem -> Mem
execStmt = undefined

------------------------------------------------------------

execProg :: Prog -> Mem -> Mem
execProg = undefined

------------------------------------------------------------

execRepeat :: Nat -> Stmt -> Mem -> Mem
execRepeat = undefined

------------------------------------------------------------

-- M.insert :: Var -> Nat -> Mem -> Mem

memLookup v m = fromMaybe Z (M.lookup v m)