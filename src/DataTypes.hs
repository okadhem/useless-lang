module DataTypes where



type Symbol = String

data Sexp = Cell Atom | List [Sexp] deriving (Show, Eq)
data Atom =  Id Symbol | Keyword Keyword deriving (Show, Eq)
data Keyword = Lambda | Column deriving (Show,Eq)

data Exp = Var Symbol TExp
         | LApp Exp Exp TExp
         | LExp Exp (Symbol,TExp) TExp
         deriving (Show,Eq)
     
data TExp = Single | Arr TExp TExp deriving (Show,Eq)
