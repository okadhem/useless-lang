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


data Exp l e = Var e
             | App (l,Exp l e) (l,Exp l e)
             | Lam TExp Name (l,Scope () (Exp l) e)
             | Zero
             | Succ (l,Exp l e)
             | Rec_Nat (l,Exp l e) Name Name (l,Scope Bool (Exp l) e) (l, Exp l e)
             --(rec_nat m (p fp (Succ fp)) val)
             deriving (Functor,Traversable,Foldable)


deriveShow1 ''Exp
instance (Show l,Show e) => Show (Exp l e) where showsPrec = showsPrec1



instance Eq1 (Exp l) where
    liftEq e (Var a) (Var a') = e a a' 
    liftEq e (App (la,a) (lb,b)) (App (la',a') (lb',b')) = liftEq e a a' && liftEq e b b'
    liftEq e (Lam ty _ (lbody,body)) (Lam ty' _ (lbody',body')) = ty == ty' && liftEq e body body'
    liftEq e Zero Zero = True
    liftEq e (Succ (la,a)) (Succ (la',a')) = liftEq e a a' 
    liftEq e (Rec_Nat (_,zero_c) _ _ (_,succ_c) (_,x)) (Rec_Nat (_,zero_c') _ _ (_,succ_c') (_,x')) 
        = liftEq e zero_c zero_c' && liftEq e succ_c succ_c' && liftEq e x x'
    liftEq e _ _ = False





instance (Eq e) => Eq (Exp l e) where (==) = eq1





instance Applicative (Exp l) where
    pure = Var
    (<*>) = ap



instance Monad (Exp l) where
    Var a >>= f =  f a
    App (l1,e1) (l2,e2) >>= f = App (l1,e1 >>= f) (l2,e2 >>= f)
    Lam ty name (l,body) >>= f = Lam ty name (l,body >>>= f)
    Zero >>= _ = Zero
    Succ (l,e) >>= f = Succ (l,e >>= f)
    Rec_Nat (l_zero_c,zero_c) name1 name2 (l_succ_c,succ_c) (l_e,e) >>= f = 
        Rec_Nat (l_zero_c,zero_c >>= f) name1 name2 (l_succ_c,succ_c >>>= f) (l_e,e >>= f)

-- utilities
withoutLoc = snd
withLoc = fst

type LocExp a = Exp String a --located expressions

val_id :: LocExp String
val_id = Lam Nat "luffy" ("Zoro",abstract1 "x" (Var "x"))  

val_1 = App ("Usopp",Var "x") ("Sanji",Var "y")

val_1_1 = val_1 >>= \x -> if x == "x" then Zero else pure x

val_1_2 = val_1 >>= \x -> if x == "x" then val_id else pure x

val_2 = Lam Nat "Nami" $ ("Nojiko",abstract1 "y" val_1)

val_3 = App ("Not located",Var "z") ("Robin", Var "w")
val_3_1 = val_3 >>= \x -> if x == "z" then val_1 else pure x





