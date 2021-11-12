module Ast where 

type Program = [Definition]

type ID = String

data Definition = Def ID Expr deriving Show

data Expr   = ExprVar ID
            | ExprConstructor ID
            | ExprNumber Int
            | ExprChar Char 
            | ExprCase Expr [CaseBranch]
            | ExprLet ID Expr Expr 
            | ExprLambda ID Expr
            | ExprApply Expr Expr
            deriving Show

data CaseBranch = CaseBranch ID [ID] Expr deriving Show
