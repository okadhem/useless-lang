module DataTypes where



type Symbol = String

data Sexp = Cell Atom | List [Sexp] deriving (Show, Eq)
data Atom =  Id Symbol | Keyword Keyword deriving (Show, Eq)
data Keyword = Lambda | Column deriving (Show,Eq)

data Exp = Var Symbol
         | LApp Exp Exp
         | Lam (Symbol,TExp) Exp
         | Zero
         | Succ Exp
         deriving (Show,Eq)
     

data TExp = Single | Arr TExp TExp | Nat deriving (Show,Eq)



-- Helper functions
cellId :: Symbol -> Sexp
cellId = Cell . Id

cellKeyword :: Keyword -> Sexp
cellKeyword = Cell . Keyword

