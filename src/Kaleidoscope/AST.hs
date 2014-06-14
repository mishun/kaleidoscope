module Kaleidoscope.AST
    ( Name
    , Expr(..)
    ) where


type Name = String


data Expr = Float Double
          | Var String
          | Call Name [Expr]
          | Function Name [Name] Expr
          | Extern Name [Name]
          | BinaryOp Name Expr Expr
          | UnaryOp Name Expr
    deriving (Show, Eq, Ord)

