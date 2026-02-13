module HS2Lazy.Syntax.Lambda where

import HS2Lazy.Syntax

type Name = String

data UTerm
  = UVar Name
  | ULam Name UTerm
  | UApp UTerm UTerm
  | ULit Literal
  | UT UTerm UTerm
  deriving (Eq)

instance Show UTerm where
  show = showUTerm False

showUTerm :: Bool -> UTerm -> String
showUTerm _ (UVar x) = x
showUTerm b (ULam x e) = "\\" ++ x ++ lineOrSpace b ++ showUTerm b e
showUTerm _ (ULit l) = show l
showUTerm b (UApp e1 e2) = showUTerm b e1 ++ " " ++ showArg b e2
showUTerm _ (UT v f) = ";" ++ showArg False v ++ " " ++ showUTerm True f

lineOrSpace :: Bool -> String
lineOrSpace b = if b then "\n" else " "

showArg :: Bool -> UTerm -> String
showArg b t
  | isApp t = "`" ++ showUTerm b t
  | otherwise = showUTerm b t

isApp :: UTerm -> Bool
isApp (UApp _ _) = True
isApp _ = False

pretty :: UTerm -> String
pretty = go 0
  where
    go _ (UVar x) = x
    go _ (ULit l) = show l
    go p (ULam x body) =
      parens (p > 1) $
        "\\" ++ x ++ " " ++ go 1 body
    go p (UApp t1 t2) =
      parens (p > 2) $
        go 2 t1 ++ " " ++ go 3 t2

    parens True s = "(" ++ s ++ ")"
    parens False s = s
