module Syntax where

import Id
import Pat
import Type
import Scheme
import Pred
import Lit
import Assump

data Module = Module [ClassDef] [InstanceDef] [DataDef] [Binding]
  deriving Show
data ClassDef = ClassDef [Pred] Id Tyvar [Binding]
  deriving Show
data InstanceDef = InstanceDef [Pred] Id Type [Binding]
  deriving Show
data DataDef = DataDef Id [Tyvar] [DataAlt]
  deriving Show
data DataAlt = DataAlt Id [Type]
  deriving Show
data Binding = Implicit Id [Alt] 
             | Explicit Id Scheme [Alt]
  deriving Show

data Expr = Var   Id
          | Lit   Literal
          | Ap    Expr Expr

          | Lam   Alt
          | If    Expr Expr Expr
          | Case  Expr [(Pat, Expr)]

          | Ascribe Expr Type

          | Const Assump
          | Let [Binding] Expr
  deriving Show

data Alt = Alt [Pat] Expr
  deriving Show

