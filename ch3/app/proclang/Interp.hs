module Interp where

import Expr
import Env

--
value_of :: Exp -> Env -> ExpVal

value_of (Const_Exp n) env = Num_Val n

value_of (Var_Exp var) env = apply_env env var

value_of (Diff_Exp exp1 exp2) env =
  let num1 = expval_num (value_of exp1 env)
      num2 = expval_num (value_of exp2 env)
  in Num_Val (num1 - num2)
  
value_of (IsZero_Exp exp) env = 
  if expval_num (value_of exp env) == 0
  then Bool_Val True
  else Bool_Val False

value_of (If_Exp exp1 exp2 exp3) env =
  if expval_bool (value_of exp1 env)
  then value_of exp2 env
  else value_of exp3 env

value_of (Let_Exp var exp1 body) env =
  value_of body (extend_env var (value_of exp1 env) env)

value_of (Proc_Exp var body) env = Proc_Val (procedure var body env)

value_of (Call_Exp rator rand) env =
  apply_procedure (expval_proc (value_of rator env)) (value_of rand env)
  

--
value_of_program :: Exp -> ExpVal

value_of_program exp = value_of exp initEnv


--
initEnv = [ ("i", Num_Val 1)
          , ("v", Num_Val 5)
          , ("x", Num_Val 10)
          ]

--
apply_procedure :: Proc -> ExpVal -> ExpVal
apply_procedure (Procedure var body env) arg =
  value_of body (extend_env var arg env)
