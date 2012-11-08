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
evalBExp (BLit b)       mem = b
evalBExp (Eq exp1 exp2) mem = (evalExp exp1 mem) == (evalExp exp2 mem)
evalBExp (Lt exp1 exp2) mem = (evalExp exp1 mem) < (evalExp exp2 mem)
evalBExp (Not exp)      mem = not (evalBExp exp mem)
evalBExp (Or exp1 exp2) mem = (evalBExp exp1 mem) || (evalBExp exp2 mem)
evalBExp (And exp1 exp2) mem = (evalBExp exp1 mem) && (evalBExp exp2 mem)

------------------------------------------------------------

execStmt :: Stmt -> Mem -> Mem
execStmt (Assign v e) m = M.insert v value m where
  value = evalExp e m
execStmt (Block []) m = m
execStmt (Block (x:xs)) m = execStmt rest cur where
  rest = Block xs
  cur = execStmt x m
execStmt (If bexp s1 s2) m
  | evalBExp bexp m = execStmt s1 m
  | otherwise = execStmt s2 m
execStmt (Repeat e s) m = execRepeat repeatTimes s m where
  repeatTimes = evalExp e m
                            
------------------------------------------------------------

execProg :: Prog -> Mem -> Mem
execProg [] m = m
execProg (x:xs) m = execProg xs (execStmt x m)

------------------------------------------------------------
execRepeat :: Nat -> Stmt -> Mem -> Mem
execRepeat Z _ mem = mem
execRepeat n st mem = execRepeat minus1 st $ execStmt st mem where
  minus1 = (evalExp (Minus (Lit n) (Lit (S Z))) mem)
------------------------------------------------------------

-- M.insert :: Var -> Nat -> Mem -> Mem

memLookup v m = fromMaybe 0 (M.lookup v m)
