module Source where

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
import Expr
import StaticPrelude

-----------------------------------------------------------------------------
-- The following helper functions are used to construct sample programs; if we
-- change the representation of Expr above, then we need only change the
-- definitions of the following combinators to match, and do not need to
-- rewrite all the test code.

ap              = foldl1 Ap
evar v          = (Var v)
elit l          = (Lit l)
econst c        = (Const c)
elet e f        = foldr Let f (map toBg e)

toBg           :: [(Id, Maybe Scheme, [Alt])] -> BindGroup
toBg g          = ([(v, t, alts) | (v, Just t, alts) <- g ],
                   filter (not . null) [[(v,alts) | (v,Nothing,alts) <- g]])

pNil            = PCon nilCfun []
pCons x y       = PCon consCfun [x,y]

eNil            = econst nilCfun
eCons x y       = ap [ econst consCfun, x, y ]

{-
ecase           = Case
elambda         = Lam
eif             = If
-}

ecase d as      = elet [[ ("_case",
                           Nothing,
                           [([p],e) | (p,e) <- as]) ]]
                       (ap [evar "_case", d])
eif c t f       = ecase c [(PCon trueCfun [], t),(PCon falseCfun [], f)]
elambda alt     = elet [[ ("_lambda",
                           Nothing,
                           [alt]) ]]
                             (evar "_lambda")
eguarded :: [(Expr, Expr)] -> Expr
eguarded        = foldr (\(c,t) e -> eif c t e) efail
efail           = Const ("FAIL" :>: Forall [Star] ([] :=> TGen 0))
esign e t       = elet [[ ("_val", Just t, [([],e)]) ]] (evar "_val")

eCompFrom p e c = ap [ econst mbindMfun, e, elambda ([p],c) ]
eCompGuard e c  = eif e c eNil
eCompLet bgs c  = elet bgs c
eListRet e      = eCons e eNil

