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

evalExp :: Exp -> Mem -> Nat
evalExp = undefined

------------------------------------------------------------

evalBExp :: BExp -> Mem -> Bool
evalBExp = undefined

------------------------------------------------------------

execStmt :: Stmt -> Mem -> Mem
execStmt = undefined

------------------------------------------------------------

execProg :: Prog -> Mem -> Mem
execProg [] m = m
execProg (x:xs) m = execProg xs (execStmt x m)

------------------------------------------------------------

execRepeat :: Nat -> Stmt -> Mem -> Mem
execRepeat = undefined

------------------------------------------------------------

-- M.insert :: Var -> Nat -> Mem -> Mem

memLookup v m = fromMaybe 0 (M.lookup v m)
