{
module Parser where

import Tokens
import Lexer
import Ast
}

%name flecha
%tokentype { Token }
%error { parseError }


%token 
      let         { TokenLet }
      in          { TokenIn }
      number      { TokenNumber $$ }
      lowerid     { TokenLowerId $$ }
      eq          { TokenEq }
      plus        { TokenPlus }
      minus       { TokenMinus }
      times       { TokenTimes }
      div         { TokenDiv }
      '('         { TokenLParen }
      ')'         { TokenRParen }
      def         { TokenDef }
      upperid     { TokenUpperId $$ }
      char        { TokenChar $$ }
      string      { TokenString $$ }
      if          { TokenIf }
      then        { TokenThen }
      else        { TokenElse }
      elif        { TokenElif }
      case        { TokenCase }
      int         { TokenIn }
      '='         { TokenDefEq }
      ';'         { TokenSemicolon }
      lambda      { TokenLambda }
      '|'         { TokenPipe  }
      arrow       { TokenArrow }
      and         { TokenAnd }
      or          { TokenOr }
      not         { TokenNot }
      ne          { TokenNe }
      ge          { TokenGe }
      le          { TokenLe }
      gt          { TokenGt }
      lt          { TokenLt }
      mod         { TokenMod }
      
%%

Program     : Program Definition    { $1 ++ [$2] }
            | {- empty -}           { [] }

Definition  : def lowerid Parameters '=' Expression   { Def $2 (mkLambda $3 $5) }

Parameters  : lowerid  Parameters               { [$1] ++ $2 }
            | {- empty -}                       { [] }

Expression  : ExternalExpression                { $1 }
            | ExternalExpression ';' Expression { ExprLet "_" $1 $3 }

ExternalExpression      : IfExpression          { $1 }
                        | CaseExpression        { $1 }
                        | LetExpression         { $1 }
                        | LambdaExpression      { $1 }
                        | InternalExpression    { $1 }

IfExpression      : if InternalExpression then InternalExpression ElseBranches      { mkIfAsCase $2 $4 $5 }

ElseBranches      : elif  InternalExpression then InternalExpression ElseBranches   { mkElseBranches (mkIfAsCase $2 $4 $5)  }
                  | else InternalExpression                                         { mkElseBranches $2 }

CaseExpression    : case InternalExpression CaseBranches                            { ExprCase $2 $3 }

CaseBranches      : CaseBranch CaseBranches                                         { [$1] ++ $2 } 
                  | {- empty -}                                                     { [] }

CaseBranch        : '|' upperid Parameters arrow InternalExpression                 { CaseBranch $2 $3 $5 }

LetExpression     : let lowerid Parameters '=' InternalExpression in ExternalExpression { ExprLet $2 (mkLambda $3 $5) $7 }

LambdaExpression  : lambda Parameters arrow ExternalExpression                      { mkLambda $2 $4 }

InternalExpression      : UnaryOperator InternalExpression                          { ExprApply $1 $2 }
                        | ApplicationExpression                                     { $1 }
                        | InternalExpression BinaryOperator InternalExpression      { ExprApply (ExprApply $2 $1) $3 }


BinaryOperator    : and   { ExprVar "AND"}
                  | or    { ExprVar "OR"}
                  | eq    { ExprVar "EQ"}
                  | ne    { ExprVar "NE"}
                  | ge    { ExprVar "GE"}
                  | le    { ExprVar "LE"}
                  | gt    { ExprVar "GT"}
                  | lt    { ExprVar "LT"}
                  | plus  { ExprVar "ADD"}
                  | minus { ExprVar "SUB"}
                  | times { ExprVar "MUL"}
                  | div   { ExprVar "DIV"}
                  | mod   { ExprVar "MOD"}
                  
UnaryOperator     : not   { ExprVar "NOT"}
                  | minus { ExprVar "UMINUS"}

ApplicationExpression   : AtomicExpression                        { $1 }
                        | ApplicationExpression AtomicExpression  {  ExprApply $1 $2 }

AtomicExpression        : lowerid   { ExprVar $1 }
                        | upperid   { ExprConstructor $1}
                        | number    { ExprNumber $1}
                        | char      { ExprChar $1}
                        | string    { mkStringAsCons $1 }
                        | '(' Expression ')' { $2 }

{
parseError :: [Token] -> a
parseError s = error $ "Parse error:\n\t+ " ++ (show s) 

-- Definitions 

mkStringAsCons :: [Char] -> Expr
mkStringAsCons [] = ExprConstructor "Nil"
mkStringAsCons (c:cs) = ExprApply (ExprApply (ExprConstructor "Cons") (ExprChar c))  (mkStringAsCons cs) 

mkLambda :: [String] -> Expr -> Expr
mkLambda [] expr = expr
mkLambda (p:ps) expr = ExprLambda p (mkLambda ps expr)

mkIfAsCase :: Expr -> Expr -> [CaseBranch] -> Expr
mkIfAsCase condExpr thenExpr elseBranches = ExprCase condExpr ((CaseBranch "True" [] thenExpr):elseBranches)

mkElseBranches :: Expr -> [ CaseBranch ]
mkElseBranches elseExpr = [ CaseBranch "False" [] elseExpr ]
}

