module Run where

import System.Environment
import Debug.Trace (trace)

import Data.Graph
import Data.List (nub)

import Parser (program, parseTokens)
import Syntax

import Id
import Assump
import qualified Expr as E
import qualified Data.Map as M
import Pred (EnvTransformer, Qual(..), (<:>), initialEnv, addClass, addInst, Pred(..), insts, ClassEnv(..))
import Type (Type(..), fn, Tycon(..))
import Kind
import Scheme (toScheme, Scheme(..))
import Pat
import TIProg (tiProgram')

imports    :: [Assump]
imports     = []

tiFile file = do
  input <- readFile file
  tiString input

tiString input =
  case parseTokens input of
        Left error -> putStrLn $ "lex error: " ++ error
        Right tokens -> do
            putStrLn "scanned"
            print tokens
            case program tokens of
               Left error -> putStrLn $ "parse error: " ++ error
               Right e -> do
                  putStrLn "parsed"
                  print e
                  let Just static = (moduleClasses e) initialEnv
                  print $ M.lookup "C" (classes static)
                  let bgs = moduleDefns e
                  print $ tiProgram' static imports bgs

moduleDefns :: Module -> [E.BindGroup]
moduleDefns (Module classes instances datas bindings) =
  [makeBindGroup $ concatMap instDefns instances ++ concatMap dataDefns datas ++ bindings]
  where 
    -- TODO: add constraints to instance definitions
    instDefns :: InstanceDef -> [Binding]
    instDefns (InstanceDef ipreds iid ity ibindings) = ibindings
    
    dataDefns :: DataDef -> [Binding]
    dataDefns (DataDef id vars alts) = map (altDefn id vars) alts

    altDefn id vars (DataAlt x tys) = trace (show ty) $ Explicit x scheme []
      where
        ty = foldr fn (foldl TAp (TCon (Tycon id kind)) (map TVar vars)) tys
        scheme = toScheme ty
        kind = foldr (const $ Kfun Star) Star vars

    -- TODO: find SCCs to create binding groups.
    -- Use Data.Graph or Algebra.Graph
    makeBindGroup :: [Binding] -> E.BindGroup
    makeBindGroup bindings = (expls, implss)
      where
        expls = do { Explicit x sc alts <- bindings; return (x, sc, map cvtAlt alts) }
        impls = do { Implicit x alts    <- bindings; return (x,     map cvtAlt alts) }

        implss = map unpackSCC (stronglyConnComp (toGraph impls))
        toGraph impls = map (\(x, alts) -> ((x, alts), x, nub $ concatMap depsAlt alts)) impls
        unpackSCC (AcyclicSCC v) = [v]
        unpackSCC (CyclicSCC vs) = vs

    depsAlt :: E.Alt -> [Id]
    depsAlt (pats, e) = concatMap depsPat pats ++ deps e 

    depsPat :: Pat -> [Id]
    depsPat (PCon (x :>: _) ps) = x : concatMap depsPat ps
    depsPat _ = []

    cvtAlt (Alt pats e) = (pats, cvt e)

    tBool     = TCon (Tycon "Bool" Star)
    falseCfun = "False" :>: (Forall [] ([] :=> tBool))
    trueCfun  = "True" :>: (Forall [] ([] :=> tBool))

    cvt (Var x) = E.Var x
    cvt (Lit l) = E.Lit l 
    cvt (Ap e1 e2) = E.Ap (cvt e1) (cvt e2)
    cvt (Lam (Alt pats e)) = E.Lam (pats, cvt e)
    cvt (If e0 e1 e2) = E.Case (cvt e0) [(PCon trueCfun [], cvt e1), (PCon falseCfun [], cvt e2)]
    cvt (Ascribe e t) = E.Ascribe (cvt e) t
    cvt (Const a) = E.Const a
    cvt (Let bindings e) = E.Let (makeBindGroup bindings) (cvt e)

    deps :: E.Expr -> [Id]
    deps (E.Var x) = [x]
    deps (E.Lit l) = []
    deps (E.Ap e1 e2) = (deps e1) ++ (deps e2)
    deps (E.Lam alt) = depsAlt alt
    deps (E.Ascribe e t) = deps e
    deps (E.Const (x :>: _)) = [x] 
    deps (E.Let bg e) = deps e

moduleClasses :: Module -> EnvTransformer
moduleClasses (Module classes instances datas bindings) = do
  foldl addC Just classes <:> foldl addI Just instances
  where
    addC transformer (ClassDef cpreds cid cvar cbindings) = do
      let csupers = [sid | IsIn sid ty <- cpreds, ty == TVar cvar]
      transformer <:> addClass cid csupers
    addI transformer (InstanceDef ipreds iid ity ibindings) = do
      transformer <:> addInst ipreds (IsIn iid ity)
