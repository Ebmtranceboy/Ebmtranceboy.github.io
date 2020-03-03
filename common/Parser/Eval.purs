module Parser.Eval where

import Prelude

import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import Parser.Error (Expect, unknownValue)
import Parser.Syntax (class Powerable, pow
                     , class Real, log, sqrt
                     , exp, sin, cos, tan, sinh, cosh, tanh
                     , asin, acos, atan, asinh, acosh, atanh
                     , Dual, Expr(..), Binop(..), Unop(..))

type Env = Map String Expr

eval :: Env -> Expr -> Expect Expr
eval env = case _ of
  d@(Lit _) -> pure d
  Var name -> case lookup name env of
    Just val -> pure val
    _ -> unknownValue name
  Binop op e1 e2 -> evalBinop env op e1 e2
  Unop op e -> evalUnop env op e

raiseDual :: Dual -> Expect Expr
raiseDual = pure <<< Lit

evalBinop :: Env -> Binop -> Expr -> Expr -> Expect Expr
evalBinop env op e1 e2 = case op of
  Add -> evalArithBinop add env e1 e2
  Sub -> evalArithBinop sub env e1 e2
  Mul -> evalArithBinop mul env e1 e2
  Div -> evalArithBinop div env e1 e2
  Pow -> evalArithBinop pow env e1 e2

type EvalBinop = Env -> Expr -> Expr -> Expect Expr

evalArithBinop :: (forall a. Ring a => Semiring a
                                    => EuclideanRing a
                                    => Powerable a
                                    => Real a => a -> a -> a) -> EvalBinop
evalArithBinop op env e1 e2 = case e1, e2 of
  Lit x, Lit y -> raiseDual $ op x y
  _, _ -> do
    e <- eval env e1
    e' <- eval env e2
    evalArithBinop op env e e'

type EvalUnop = Env -> Expr -> Expect Expr

evalUnop :: Env -> Unop -> Expr -> Expect Expr
evalUnop env op e = case op of
  Negate -> evalReal negate env e
  Sqrt -> evalReal sqrt env e
  Exp -> evalReal exp env e
  Sin -> evalReal sin env e
  Cos -> evalReal cos env e
  Tan -> evalReal tan env e
  Sinh -> evalReal sinh env e
  Cosh -> evalReal cosh env e
  Tanh -> evalReal tanh env e
  Asin -> evalReal asin env e
  Acos -> evalReal acos env e
  Atan -> evalReal atan env e
  Asinh -> evalReal asinh env e
  Acosh -> evalReal acosh env e
  Atanh -> evalReal atanh env e
  Log -> evalReal log env e

evalNegate :: EvalUnop
evalNegate env = case _ of
  Lit d -> raiseDual (-d)
  e -> eval env e >>= evalNegate env

evalReal :: (Dual -> Dual) -> EvalUnop
evalReal op env = case _ of
  Lit d -> raiseDual (op d)
  e -> eval env e >>= evalReal op env
