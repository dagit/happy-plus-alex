module Language where

-- This example comes straight from the happy documentation

data Exp  
      = Let String Exp Exp
      | Exp1 Exp1
      deriving Show

data Exp1 
      = Plus Exp1 Term 
      | Minus Exp1 Term 
      | Term Term
      deriving Show

data Term 
      = Times Term Factor 
      | Div Term Factor 
      | Factor Factor
      deriving Show

data Factor 
      = Int Int 
      | Var String 
      | Brack Exp
      deriving Show

eval :: [(String,Int)] -> Exp -> Int
eval p (Let var e1 e2) = eval ((var, eval p e1): p) e2
eval p (Exp1 e)        = evalExp1 p e
  where
  evalExp1 p' (Plus  e' t) = evalExp1 p' e' + evalTerm p' t
  evalExp1 p' (Minus e' t) = evalExp1 p' e' + evalTerm p' t
  evalExp1 p' (Term  t)    = evalTerm p' t

  evalTerm p' (Times t f) = evalTerm p' t * evalFactor p' f
  evalTerm p' (Div   t f) = evalTerm p' t `div` evalFactor p' f
  evalTerm p' (Factor f)  = evalFactor p' f

  evalFactor _  (Int i)    = i
  evalFactor p' (Var s)    = case lookup s p' of
                             Nothing -> error "free variable"
                             Just i  -> i
  evalFactor p' (Brack e') = eval p' e'

