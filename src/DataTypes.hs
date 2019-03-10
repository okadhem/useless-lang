module DataTypes where



type Symbol = String


data Exp = Var Symbol
         | LApp Exp Exp
         | Lam (Symbol,TExp) Exp
         | Zero
         | Succ Exp
         | Rec_Nat Exp Symbol Symbol Exp Exp
         deriving (Show,Eq)
     

data TExp = Single | Arr TExp TExp | Nat deriving (Show,Eq)



