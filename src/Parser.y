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

Program                 : Program Definition                                  { $1 ++ [$2] }
                        | {- empty -}                                         { [] }

Definition              : def lowerid Parameters '=' Expression               { Def $2 (mkLambda $3 $5) }

Parameters              : lowerid  Parameters                                 { [$1] ++ $2 }
                        | {- empty -}                                         { [] }

Expression              : ExternalExpression                                  { $1 }
                        | ExternalExpression ';' Expression                   { ExprLet "_" $1 $3 }

ExternalExpression      : IfExpression                                        { $1 }
                        | CaseExpression                                      { $1 }
                        | LetExpression                                       { $1 }
                        | LambdaExpression                                    { $1 }
                        | InternalExpression                                  { $1 }

IfExpression            : if InternalExpression then InternalExpression ElseBranches   { mkIfAsCase $2 $4 $5 }

ElseBranches            : elif InternalExpression then InternalExpression ElseBranches { mkElseBranches (mkIfAsCase $2 $4 $5)  }
                        | else InternalExpression                                      { mkElseBranches $2 }

CaseExpression          : case InternalExpression CaseBranches                { ExprCase $2 $3 }

CaseBranches            : CaseBranch CaseBranches                             { [$1] ++ $2 } 
                        | {- empty -}                                         { [] }

CaseBranch              : '|' upperid Parameters arrow InternalExpression     { CaseBranch $2 $3 $5 }

LetExpression           : let lowerid Parameters '=' InternalExpression in ExternalExpression { ExprLet $2 (mkLambda $3 $5) $7 }

LambdaExpression        : lambda Parameters arrow ExternalExpression          { mkLambda $2 $4 }

InternalExpression      : AssociativeExpression                               { $1 }

AssociativeExpression   : BinaryExpressionOR                                  { $1 }

BinaryExpressionOR      : BinaryExpressionOR or BinaryExpressionAnd           { mkBinOperation "OR" $1 $3 }
                        | BinaryExpressionAnd                                 { $1 }

BinaryExpressionAnd     : BinaryExpressionAnd and UnaryExpresionNot           { mkBinOperation "AND" $1 $3 }
                        | UnaryExpresionNot                                   { $1 }

UnaryExpresionNot       : not UnaryExpresionNot                               { ExprApply (ExprVar "NOT") $2}
                        | OrdinalExpression                                   { $1 }

OrdinalExpression       : OrdinalExpression eq AdditiveExpression             { mkBinOperation "EQ" $1 $3 }
                        | OrdinalExpression ne AdditiveExpression             { mkBinOperation "NE" $1 $3 }
                        | OrdinalExpression ge AdditiveExpression             { mkBinOperation "GE" $1 $3 }
                        | OrdinalExpression le AdditiveExpression             { mkBinOperation "LE" $1 $3 }
                        | OrdinalExpression gt AdditiveExpression             { mkBinOperation "GT" $1 $3 }
                        | OrdinalExpression lt AdditiveExpression             { mkBinOperation "LT" $1 $3 }
                        | AdditiveExpression                                  { $1 }

AdditiveExpression      : AdditiveExpression plus TimesExpression             { mkBinOperation "ADD" $1 $3 }
                        | AdditiveExpression minus TimesExpression            { mkBinOperation "SUB" $1 $3 }
                        | TimesExpression                                     { $1 }

TimesExpression         : TimesExpression times DivExpression                 { mkBinOperation "MUL" $1 $3 }
                        | DivExpression                                       { $1 }

DivExpression           : DivExpression div UminusExpression                  { mkBinOperation "DIV" $1 $3 }
                        | DivExpression mod UminusExpression                  { mkBinOperation "MOD" $1 $3 }
                        | UminusExpression                                    { $1 }

UminusExpression        : minus UminusExpression                              { ExprApply (ExprVar "UMINUS") $2}
                        | AtomicExpression                                    { $1 }
                        | ApplicationExpression                               { $1 }

ApplicationExpression   : AtomicExpression                                    { $1 }
                        | ApplicationExpression AtomicExpression              {  ExprApply $1 $2 }

AtomicExpression        : lowerid                                             { ExprVar $1 }
                        | upperid                                             { ExprConstructor $1}
                        | number                                              { ExprNumber $1}
                        | char                                                { ExprChar $1}
                        | string                                              { mkStringAsCons $1 }
                        | '(' Expression ')'                                  { $2 }

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

mkBinOperation op left right = ExprApply (ExprApply (ExprVar op) left) right
}

