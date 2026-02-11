module HS2Lazy.Syntax.Lambda where

import HS2Lazy.Syntax

type Name = String

data UTerm
  = UVar Name
  | ULam Name UTerm
  | UApp UTerm UTerm
  | ULit Literal
  deriving (Eq, Show)
