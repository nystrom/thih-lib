{

{-

All syntactic sugar has been compiled away. So, no lists, no tuples.
The only literals are the primitive literals (int, rat, string, char).

-}


module Parser(
  program,
  parseTokens
) where

import Debug.Trace (trace)

import Lexer
import Syntax
import Id
import Pat
import Type
import Scheme
import Pred
import Lit
import Kind
import Assump

}

%monad {Either String} { (>>=) } {return}

%name program Module
%tokentype { Token }
%error     { parseError }

%token
  class                        { TokenClass }
  instance                     { TokenInstance }
  data                         { TokenData }
  fun                          { TokenFun }
  let                          { TokenLet }
  in                           { TokenIn }
  case                         { TokenCase }
  of                           { TokenOf }
  forall                       { TokenForall }
  if                           { TokenIf }
  then                         { TokenThen }
  else                         { TokenElse }
  NUM                          { TokenNum $$ }
  STRING                       { TokenString $$ }
  '='                          { TokenBind }
  '+'                          { TokenAdd }
  '-'                          { TokenSub }
  '*'                          { TokenMul }
  '/'                          { TokenDiv }
  '++'                         { TokenAppend }
  '<'                          { TokenLt }
  '>'                          { TokenGt }
  '<='                         { TokenLe }
  '>='                         { TokenGe }
  '=='                         { TokenEqu }
  '/='                         { TokenNeq }
  '&&'                         { TokenAnd }
  '||'                         { TokenOr }
  '|'                          { TokenAlt }
  '!'                          { TokenBang }
  '->'                         { TokenArrow }
  '=>'                         { TokenDoubleArrow }
  '('                          { TokenLParen }
  ')'                          { TokenRParen }
  '{'                          { TokenLCurly }
  '}'                          { TokenRCurly }
  '['                          { TokenLBrack }
  ']'                          { TokenRBrack }
  ';'                          { TokenSemi }
  ','                          { TokenComma }
  '.'                          { TokenDot }
  '::'                         { TokenAscribe }
  ':'                          { TokenColon }
  '@'                          { TokenAt }
  '_'                          { TokenWildcard }
  '?'                          { TokenDyn }
  VAR                          { TokenVar $$ }
  CON                          { TokenCon $$ }

%nonassoc  let if fun class instance data case
%right of in then else '=' '->'
%right     '::'
%left     '||'
%left     '&&'
%nonassoc  '/=' '==' '<' '>' '>=' '<='
%right     '++'
%right     ':'
%left      '+' '-'
%left      '*' '/' '%'
%nonassoc  NEG '!'
%%

Module         
  : Classes Instances Datas Bindings                  { Module $1 $2 $3 $4 }

Classes
  : Class Classes                                     { $1 : $2 }
  |                                                   { [] }

Instances
  : Instance Instances                                { $1 : $2 }
  |                                                   { [] }

Datas
  : Data Datas                                        { $1 : $2 }
  |                                                   { [] }

Class 
  : class Pred '=>' CON TyVar '{' Bindings '}'        { ClassDef $2 $4 $5 $7 }
  | class CON TyVar '{' Bindings '}'                  { ClassDef [] $2 $3 $5 }

Instance
  : instance Pred '=>' CON Type '{' Bindings '}'      { InstanceDef $2 $4 $5 $7 }
  | instance CON Type '{' Bindings '}'                { InstanceDef [] $2 $3 $5 }

Data
  : data CON TyVars '=' Cases ';'                     { DataDef $2 $3 $5 }

Cases
  : Case '|' Cases                                    { $1 : $3 }
  | Case                                              { [$1] }

Case
  : CON Types                                         { DataAlt $1 $2 }

TyVars 
  : TyVar TyVars                                      { $1 : $2 }
  |                                                   { [] }

TyVar 
  : VAR                                               { Tyvar $1 Star }
  | '(' VAR ')'                                       { Tyvar $2 Star }
  | '(' VAR '::' Kind ')'                             { Tyvar $2 $4 }

TyCon 
  : CON                                               { Tycon $1 Star }
  | '(' CON ')'                                       { Tycon $2 Star }
  | '(' CON '::' Kind ')'                             { Tycon $2 $4 }

Kind
  : '*'                                               { Star }
  | Kind '->' Kind                                    { Kfun $1 $3 }
  | '(' Kind ')'                                      { $2 }

Types 
  : Type Types                                        { $1 : $2 }
  |                                                   { [] }

Types1 
  : Type Types                                        { $1 : $2 }
  | Type                                              { [$1] }

Type    
  : TyVar                                             { TVar $1 }
  | TyCon                                             { TCon $1 }
  | '(' TyCon Types1 ')'                              { foldr TAp (TCon $2) $3 }
  | Type '->' Type                                    { $1 `fn` $3 }                     
  | '(' TypeCommas ')'                                { tTuple $2 }
  | '[' Type ']'                                      { TAp tList $2 }
  | '(' ')'                                           { TCon (Tycon "()" Star) }
  | '?'                                               { TCon (Tycon "?" Star) }  -- dynamic type

TypeCommas
  : Type ',' TypeCommas                               { $1 : $3 }
  | Type                                              { [$1] }

ExprCommas
  : Expr ',' ExprCommas                               { $1 : $3 }
  | Expr                                              { [$1] }

Bindings 
  : Binding ';' Bindings                              { $1 : $3 }
  |                                                   { [] }

Binding 
  : Implicit                                          { $1 }
  | Explicit                                          { $1 }

Implicit 
  : VAR '|' Alts                                      { Implicit $1 $3 }
  | VAR '=' Expr                                      { Implicit $1 [Alt [] $3] }

Alts 
  : Alt '|' Alts                                      { $1 : $3 }
  | Alt                                               { [$1] }

Alt 
  : Pats '=' Expr                                     { Alt $1 $3 }

Explicit 
  : VAR '::' Scheme                                   { Explicit $1 $3 [] }
  | VAR '::' Scheme '|' Alts                          { Explicit $1 $3 $5 }
  | VAR '::' Scheme '=' Expr                          { Explicit $1 $3 [Alt [] $5] }

Scheme 
  : forall TyVars '.' Type                            { quantify $2 ([] :=> $4) }
  | forall TyVars '.' Pred '=>' Type                  { quantify $2 ($4 :=> $6) }
  | Type                                              { toScheme $1 }

Pred
  : CON Type                                          { [IsIn $1 $2] }
  | '(' PredCommas ')'                                { $2 }
  | '(' ')'                                           { [] }

PredCommas
  : CON Type ',' PredCommas                           { (IsIn $1 $2) : $4 }
  | CON Type                                          { (IsIn $1 $2): [] }

Pats 
  : Pat Pats                                          { $1 : $2 }
  |                                                   { [] }

Pats1 
  : Pat Pats                                          { $1 : $2 }
  | Pat                                               { [$1] }

Pat 
  : VAR                         { PVar $1 }
  | VAR '@' Pat                 { PAs $1 $3 }
  | '_'                          { PWildcard }
  | '(' CON '::' Scheme ')'                 { PCon ($2 :>: $4) [] }
  | '(' '(' CON '::' Scheme ')' Pats1 ')'        { PCon ($3 :>: $5) $7 }
  | Lit                    { PLit $1 }
  | '(' Pat ')'  { $2 }

Expr           
  : let Bindings in Expr              { Let $2 $4 }
  | fun Pats '->' Expr                 { Lam (Alt $2 $4) }
  | if Expr then Expr else Expr       { If $2 $4 $6 }
  | case Expr of CaseAlts             { Case $2 $4 }
  | case Expr of '|' CaseAlts             { Case $2 $5 }
  | Binary                            { $1 }
  
CaseAlts 
  : CaseAlt '|' CaseAlts      { $1 : $3 }
  | CaseAlt                 { [$1] }

CaseAlt
  : Pat '->' Expr          { ($1, $3) }

Binary        
  : Binary '::' Type                  { Ascribe $1 $3 }
  | Binary ':' Binary                 { Ap (Ap (Const consCfun) $1) $3 }
  | Binary '++' Binary                { Ap (Ap (Var "(++)") $1) $3 }
  | Binary '||' Binary                { If $1 (Const trueCfun) $3 }
  | Binary '&&' Binary                { If $1 $3 (Const falseCfun) }
  | Binary '/=' Binary                { Ap (Ap (Var "(/=)") $1) $3 }
  | Binary '==' Binary                { Ap (Ap (Var "(==)") $1) $3 }
  | Binary '<' Binary                 { Ap (Ap (Var "(<)") $1) $3 }
  | Binary '>' Binary                 { Ap (Ap (Var "(>)") $1) $3 }
  | Binary '>=' Binary                { Ap (Ap (Var "(>=)") $1) $3 }
  | Binary '<=' Binary                { Ap (Ap (Var "(<=)") $1) $3 }
  | Binary '+' Binary                 { Ap (Ap (Var "(+)") $1) $3 }
  | Binary '-' Binary                 { Ap (Ap (Var "(-)") $1) $3 }
  | Binary '*' Binary                 { Ap (Ap (Var "(*)") $1) $3 }
  | '-' Binary     %prec NEG          { Ap (Var "negate") $2 }
  | '!' Binary     %prec NEG          { Ap (Var "not") $2 }
  | Term                              { $1 }

Term           
  : Term Factor                       { Ap $1 $2 }
  | Factor                            { $1 }

Factor         
  : '(' ExprCommas ')'                      { tuple $2 }
  | Lit                               { Lit $1 }
  | VAR                               { Var $1 }
  | '(' CON '::' Scheme ')'            { Const ($2 :>: $4) }
  | '(' ')'                            { Const unitCfun }
  | '[' ']'                            { Const nilCfun }

Lit 
  : NUM                               { LitInt $1 }

{

parseError (l:ls) = Left (show l)
parseError [] = Left "Unexpected end of input"

parseTokens :: String -> Either String [Token]
parseTokens s = Right $ scanTokens s

unitCfun = "()" :>: (Forall [] ([] :=> tUnit))

nilCfun  = "[]" :>: (Forall [Star] ([] :=> (TAp tList (TGen 0))))
consCfun = ":"  :>: (Forall [Star]
                     ([] :=> 
                      (TGen 0 `fn`
                       TAp tList (TGen 0) `fn`
                       TAp tList (TGen 0))))

tBool     = TCon (Tycon "Bool" Star)
falseCfun = "False" :>: (Forall [] ([] :=> tBool))
trueCfun  = "True" :>: (Forall [] ([] :=> tBool))

tTuple [] = tUnit
tTuple [ty] = ty
tTuple tys = foldl TAp (TCon (Tycon name kind)) tys
  where
    kind = foldr (\t k -> Kfun Star k) Star tys
    name = "(" ++ (tail $ map (\_ -> ',') tys) ++ ")"

tuple [] = Const unitCfun
tuple [x] = x
tuple xs = foldl Ap (Const (name :>: Forall (map (const Star) xs) ty)) xs
  where
    ty = [] :=> tTuple (zipWith (const TGen) xs [0..])
    name = "(" ++ (tail $ map (\_ -> ',') xs) ++ ")"

{-
pTuple [] = PCon unitCfun []
tuple [x] = x
tuple xs = foldl Ap (Const (name :>: Forall (map (const Star) xs) ty)) xs
  where
    ty = [] :=> tTuple (zipWith (const TGen) xs [0..])
    name = "(" ++ (tail $ map (\_ -> ',') xs) ++ ")"
-}

}
