module TypeCheck where

import qualified Data.Map as Map
import Expr

--
typeCheck :: Exp -> IO (Either String Type)
typeCheck exp = return (type_of_program exp )

--
initTyEnv :: TyEnv
initTyEnv = extend_tyenv "i" TyInt
            (extend_tyenv "v" TyInt
              (extend_tyenv "x" TyInt empty_tyenv))
     
--
type_of_program :: Exp -> Either String Type
type_of_program exp = type_of exp initTyEnv

--
type TyEnv = Map.Map Identifier Type

type_of :: Exp -> TyEnv -> Either String Type

type_of (Const_Exp n) tyenv = Right TyInt

type_of (Var_Exp var) tyenv = apply_tyenv tyenv var

type_of (Diff_Exp exp1 exp2) tyenv = 
  do ty1 <- type_of exp1 tyenv 
     ty2 <- type_of exp2 tyenv 
     if equalType ty1 TyInt
     then if equalType ty2 TyInt 
          then Right TyInt 
          else expectedButErr TyInt ty2 exp2 
     else expectedButErr TyInt ty1 exp1

type_of (IsZero_Exp exp1) tyenv = 
  do ty1 <- type_of exp1 tyenv 
     if equalType ty1 TyInt 
     then Right TyBool 
     else expectedButErr TyInt ty1 exp1

type_of (If_Exp exp1 exp2 exp3) tyenv =
  do ty1 <- type_of exp1 tyenv 
     ty2 <- type_of exp2 tyenv 
     ty3 <- type_of exp3 tyenv 
     if equalType ty1 TyBool 
     then if equalType ty2 ty3 
          then Right ty2 
          else inequalIfBranchTyErr ty2 ty3 exp2 exp3 
     else expectedButErr TyBool ty1 exp1

type_of (Let_Exp var exp1 body) tyenv =
  do ty1 <- type_of exp1 tyenv 
     ty2 <- type_of body (extend_tyenv var ty1 tyenv) 
     Right ty2

type_of (Letrec_Exp ty proc_name bound_var bvar_ty proc_body letrec_body) tyenv =
  do let tyenv1 = extend_tyenv bound_var bvar_ty (extend_tyenv proc_name (TyFun bvar_ty ty) tyenv)
     ty1 <- type_of proc_body tyenv1 
     let tyenv2 = extend_tyenv proc_name (TyFun bvar_ty ty) tyenv 
     ty2 <- type_of letrec_body tyenv
     if equalType ty1 ty 
     then Right ty2 
     else expectedButErr ty ty1 proc_body

type_of (Proc_Exp var argTy body) tyenv = 
  do ty1 <- type_of body (extend_tyenv var argTy tyenv)
     Right (TyFun argTy ty1)

type_of (Call_Exp rator rand) tyenv =
  do ty1 <- type_of rator tyenv 
     ty2 <- type_of rand tyenv 
     case ty1 of TyFun ty3 ty4 -> if equalType ty2 ty3 
                                  then Right ty4
                                  else inequalArgtyErr ty3 ty2 rator rand 
                 _             -> expectedFuntyButErr ty1 rator

         
-- Utilities
apply_tyenv :: TyEnv -> Identifier -> Either String Type 
apply_tyenv tyenv var =
  case Map.lookup var tyenv of
    Just ty -> Right ty
    Nothing -> Left $ "Variable not found: " ++ var

empty_tyenv :: TyEnv 
empty_tyenv = Map.empty 

extend_tyenv :: Identifier -> Type -> TyEnv -> TyEnv
extend_tyenv var ty tyenv = Map.insert var ty tyenv

expectedButErr :: Type -> Type -> Exp -> Either String Type
expectedButErr expectedTy gotTy exp =
  Left $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr :: Type -> Exp -> Either String Type
expectedFuntyButErr gotTy exp =
  Left $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

inequalIfBranchTyErr :: Type -> Type -> Exp -> Exp -> Either String Type
inequalIfBranchTyErr thenTy elseTy exp2 exp3 =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show thenTy ++ " in " ++ show exp2
          ++ "\t" ++ show elseTy ++ " in " ++ show exp3

inequalArgtyErr :: Type -> Type -> Exp -> Exp -> Either String Type
inequalArgtyErr argTy1 argTy2 funexp argexp =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show argTy1 ++ " for the arugment of " ++ show funexp
          ++ "\t" ++ show argTy2 ++ " in " ++ show argexp

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType (TyFun ty1 ty1') (TyFun ty2 ty2') =
  equalType ty1 ty2 && equalType ty1' ty2'
equalType _ _ = False

