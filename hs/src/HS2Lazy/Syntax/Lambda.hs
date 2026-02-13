module HS2Lazy.Syntax.Lambda where

import HS2Lazy.Syntax

type Name = String

data UTerm
  = UVar Name
  | ULam Name UTerm
  | UApp UTerm UTerm
  | ULit Literal
  | UT UTerm UTerm
  deriving (Eq, Show)

prettyLambda :: UTerm -> String
prettyLambda term = go 0 term
  where
    indent :: Int -> String
    indent n = replicate (4 * n) ' '

    go :: Int -> UTerm -> String
    go lvl (UVar name) = indent lvl ++ "UVar " ++ show name
    go lvl (ULit lit) = indent lvl ++ "ULit " ++ show lit
    go lvl (ULam n t) =
      indent lvl ++ "ULam " ++ show n ++ "\n" ++ go (lvl + 1) t
    go lvl (UApp t1 t2) =
      indent lvl ++ "UApp\n" ++ go (lvl + 1) t1 ++ "\n" ++ go (lvl + 1) t2
    go lvl (UT t1 t2) =
      indent lvl ++ "UT\n" ++ go (lvl + 1) t1 ++ "\n" ++ go (lvl + 1) t2

parens :: Bool -> String -> String
parens True s = "(" ++ s ++ ")"
parens False s = s

prettyUTerm :: UTerm -> String
prettyUTerm = go False
  where
    go :: Bool -> UTerm -> String
    go _ (UVar x) = x
    go b (ULit l) = show l
    go b (ULam x body) = "\\" ++ x ++ " " ++ go b body
    go b (UApp e1 e2) =
      let left = go b e1
          right = goArg e2
       in left ++ " " ++ right
    go b (UT v f) =
      "T " ++ go False v ++ " " ++ go True f

    goArg :: UTerm -> String
    goArg t@(UApp _ _) = "`" ++ go False t
    goArg t = go False t

showFruit :: UTerm -> String
showFruit = showFruitFix False

showFruitFix :: Bool -> UTerm -> String
showFruitFix _ (UVar x) = x
showFruitFix b (ULam x e) = "\\" ++ x ++ lineOrSpace b ++ showFruitFix False e
showFruitFix _ (ULit l) = show l
showFruitFix b (UApp e1 e2) = showFruitFix b e1 ++ " " ++ showArg b e2
showFruitFix _ (UT v f) = ";" ++ showArg False v ++ " " ++ showFruitFix True f

lineOrSpace :: Bool -> String
lineOrSpace b = if b then "\n" else " "

showArg :: Bool -> UTerm -> String
showArg b t
  | isApp t = "`" ++ showFruitFix b t
  | otherwise = showFruitFix b t

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
