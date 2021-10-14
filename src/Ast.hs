module Ast where 



type Program = [Definition]

type Id = String
type ConsId = String

data Definition = Def Id Expr 
                    deriving Show

data Expr   = ExprVar Id
            | ExprConstructor ConsId 
            | ExprNumber Integer 
            | ExprChar Char 
            | ExprCase Expr [CaseBranch]
            | ExprLet Id Expr Expr 
            | ExprLambda Id Expr
            | ExprApply Expr Expr
            deriving Show

data CaseBranch = CaseBranch Id [Id] Expr 
                    deriving Show
