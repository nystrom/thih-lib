module Expr where
import Id
import Kind
import Type
import Subst
import Pred
import Scheme
import Assump
import TIMonad
import Infer
import Lit
import Pat

-----------------------------------------------------------------------------

data Expr = Var   Id
          | Lit   Literal
          | Const Assump
          | Ap    Expr Expr
          | Let   BindGroup Expr

          | Lam   Alt
          | Case  Expr [(Pat,Expr)]

-----------------------------------------------------------------------------

type Alt = ([Pat], Expr)

-----------------------------------------------------------------------------

type Expl = (Id, Scheme, [Alt])
type Impl   = (Id, [Alt])

type BindGroup  = ([Expl], [[Impl]])


