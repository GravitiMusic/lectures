{-# OPTIONS_GHC -fwarn-tabs #-}
-- Haskell is space sensitive

{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}
-- Turn on warnings

--
-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can load this file into GHCi by typing the command `:load Examples.hs` at
-- the GHCi prompt.

module Examples where

type Var = String

data Exp = Var Var
         | Const Double
         | Add Exp Exp
         | Mul Exp Exp
         | Sub Exp Exp
         | Div Exp Exp
         | Neg Exp
  deriving (Show)

-- | Evaluate expressions
eval :: [(Var, Double)] -> Exp -> Double
eval env (Var v)     = case lookup v env of
                         Nothing -> error $ "Unbound variable: " ++ v
                         Just n  -> n
eval _   (Const n)   = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 / eval env e2
eval env (Neg e)     = - eval env e

-- Compute derivative of an expressions with respect to a variable
deriv :: Exp -> Var -> Exp
deriv (Var v) x
  | v == x          = Const 1
  | otherwise       = Const 0
deriv (Const _) _   = Const 0
deriv (Add e1 e2) x = Add (deriv e1 x) (deriv e2 x)
deriv (Mul e1 e2) x = Add (Mul e1 (deriv e2 x))
                          (Mul (deriv e1 x) e2)
deriv (Sub e1 e2) x = Sub (deriv e1 x) (deriv e2 x)
deriv (Div e1 e2) x = Div (Sub (Mul (deriv e1 x) e2)
                               (Mul e1 (deriv e2 x)))
                          (Mul e2 e2)
deriv (Neg e) x     = Neg (deriv e x)

-- | "Compile" an expression into a Haskell function
compile :: Exp -> Double -> Double
compile e x = eval [("x", x)] e

