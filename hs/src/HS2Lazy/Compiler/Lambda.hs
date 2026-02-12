module HS2Lazy.Compiler.Lambda where

import HS2Lazy.Compiler (expandCon, programToExpr)
import HS2Lazy.Syntax
import HS2Lazy.Syntax.Lambda

yComb :: UTerm
yComb =
  ULam
    "f"
    ( UApp
        ( ULam
            "x"
            ( UApp
                (UVar "f")
                (UApp (UVar "x") (UVar "x"))
            )
        )
        ( ULam
            "x"
            ( UApp
                (UVar "f")
                (UApp (UVar "x") (UVar "x"))
            )
        )
    )

compileToLambda :: Expr -> UTerm
compileToLambda (Var x) = UVar x
compileToLambda (Ap e1 e2) =
  UApp (compileToLambda e1) (compileToLambda e2)
compileToLambda (Lambda (vs, Rhs e)) =
  foldr ULam (compileToLambda e) (map patVar vs)
  where
    patVar (PVar v) = v
    patVar p = error ("Unsupported lambda pattern: " ++ show p)
compileToLambda (Let bg e) =
  compileLetGroup (bindings bg) (compileToLambda e)
compileToLambda (Lit l) = ULit l
compileToLambda (Con c) =
  error ("Constructor should be expanded before lambda compilation")
compileToLambda e =
  error ("compileToLambda: " ++ show e)

compileLetGroup :: [(Id, [Alt])] -> UTerm -> UTerm
compileLetGroup defs body =
  foldr compileOne body defs

compileOne :: (Id, [Alt]) -> UTerm -> UTerm
compileOne (name, [alt]) body =
  let rhs = compileAlt alt
   in if occurs name rhs
        then
          UApp (ULam name body) (UApp yComb (ULam name rhs))
        else
          UT rhs (ULam name body)
compileOne def _ =
  error ("Unsupported definition: " ++ show def)

compileAlt :: Alt -> UTerm
compileAlt ([], Rhs e) =
  compileToLambda e
compileAlt (PVar v : ps, rhs) =
  ULam v (compileAlt (ps, rhs))
compileAlt (p : _, _) =
  error ("Unsupported pattern in alt: " ++ show p)

occurs :: Name -> UTerm -> Bool
occurs x (UVar y) = x == y
occurs x (ULam y t)
  | x == y = False
  | otherwise = occurs x t
occurs x (UApp t1 t2) = occurs x t1 || occurs x t2
occurs _ (ULit _) = False
occurs x (UT t f) = occurs x t || occurs x f

lambdaCompile :: Program -> UTerm
lambdaCompile =
  compileToLambda
    . expandCon
    . programToExpr
