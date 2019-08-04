{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

{-# LANGUAGE TemplateHaskell #-}
module DataTypes where
import Data.Traversable
import Data.Foldable
import Control.Monad
import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes



type Symbol = String
type Name = String

data TExp =  Single
             | Arr TExp TExp 
             | Nat
             deriving (Show,Eq)

type Exp' = Exp String 

data Exp e = Var e
             | App (Exp e) (Exp e)
             | Lam TExp Name (Scope () Exp e)
             | Zero
             | Succ (Exp e)
             | Rec_Nat (Exp e) Name Name (Scope Bool Exp e) (Exp e)
	     --(rec_nat m (p fp (Succ fp)) val)
             deriving (Functor,Traversable,Foldable)


deriveShow1 ''Exp
instance (Show e) => Show (Exp e) where showsPrec = showsPrec1


deriveEq1 ''Exp
instance (Eq e) => Eq (Exp e) where (==) = eq1





instance Applicative Exp where
	pure = Var
	(<*>) = ap



instance Monad Exp where
	Var a >>= f =  f a
	App e1 e2 >>= f = App (e1 >>= f) (e2 >>= f)
	Lam ty name body >>= f = Lam ty name (body >>>= f)
        Zero >>= _ = Zero
        Succ e >>= f = Succ $ e >>= f
        Rec_Nat zero_c name1 name2 succ_c e >>= f = Rec_Nat (zero_c >>= f) name1 name2 (succ_c >>>= f) (e >>= f)







