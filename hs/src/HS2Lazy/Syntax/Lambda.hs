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
  show = showUTerm

showUTerm :: UTerm -> String
showUTerm (UVar x) = x
showUTerm (ULam x e) = "\\" ++ x ++ "\n" ++ showUTerm e
showUTerm (ULit l) = show l
showUTerm (UApp e1 e2) =
  showUTerm e1 ++ " " ++ showArg e2
showUTerm (UT v f) =
  showUTerm f ++ " " ++ showArg v

showArg :: UTerm -> String
showArg t
  | isApp t = "`" ++ showUTerm t
  | otherwise = showUTerm t

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
