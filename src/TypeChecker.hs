{-# Language ScopedTypeVariables #-}
{-# Language GADTs #-}
{-# Language DeriveFunctor  #-}

module TypeChecker where

import DataTypes
import Data.Tree
import Text.Parsec
import Bound (fromScope , toScope, Var (..))

data TypingJudgement l a = TypingJudgement (Exp l a) TExp  deriving (Show,Functor)

--instance Functor TypingJudgement where
--    fmap f (TypingJudgement e ty) = TypingJudgement (f <$> e) ty
--instance Show l => Show (TypingJudgement a) where
--    show (TypingJudgement e ty) = "TypingJudgement " ++ show e ++ " " ++ show ty


data HypotheticalTypingJudgement l a  = 
    HypotheticalTypingJudgement [TypingJudgement l a] (Exp l a) TExp deriving (Show,Functor)

--instance Functor HypotheticalTypingJudgement where
--    fmap f (HypotheticalTypingJudgement ctx j ty) = HypotheticalTypingJudgement (fmap f <$> ctx) (f <$> j) ty
--


type Derivation l = Tree (HypotheticalTypingJudgement l Forall)

type TypeError = String 

type Ctx l a = [TypingJudgement l a]



--  
-- ----------------------
--   C, x : T |- x : T

--     C,x : V |- b : U
-- --------------------------
-- C |- lambda x:V. b : V -> U


-- C |- u : t -> s    C |-  v : t
-- ------------------------------
--       C  |- u v : s

-- 
------------------------
--   C  |- Zero : nat

-- C |- n : nat
----------------------
-- C |- Succ n : nat 


-- C |- g : nat   C |- e0 : t   C,x:nat,y:t |- e : t
------------------------------------------------------
--        C |- (rec_nat e0 (x y e)) (g) : t


typeCheck :: (Show a,Eq a,Show l) => Ctx l a -> Exp l a -> Either TypeError (Derivation l,TExp)
typeCheck ctx exp@(Lam v _ body) = let
  hypothesis = TypingJudgement (Var $ B ()) v
  premise = typeCheck (liftExtend [hypothesis] ctx) (fromScope $ withoutLoc body)
  u = snd <$> premise
  cons = (\u -> HypotheticalTypingJudgement ctx exp (Arr v u)) <$> u 
  in
    do
      u' <- u
      cons' <-  cons
      premiseDer <- fst <$> premise 
      return $ (Node (E <$> cons') [premiseDer], Arr v u') 
    



typeCheck ctx (App f g) = let
  lpremise = typeCheck ctx $ withoutLoc f
  rpremise = typeCheck ctx $ withoutLoc g
  fCodomain = do
    g_ty <- snd <$> rpremise
    f_ty <- snd <$> lpremise
    case f_ty of
             Arr s t -> if s == g_ty then return $ t
                        else Left $ "then function expects arg of type" ++ show s
                             ++ "but it has been given a value of type " ++ show g_ty 
             
             _ -> Left $ show f ++ "is not of function type"

  cons = do
    t <- fCodomain
    return $ HypotheticalTypingJudgement ctx (App f g) t
    
  in 
    do
    cons' <- cons
    lpremise' <- lpremise
    rpremise' <- rpremise
    fCodomain' <- fCodomain
    return $ (Node (E <$> cons') [fst lpremise' , fst rpremise'], fCodomain') 




typeCheck ctx exp@(Var x)  = case lookfor (Var x) ctx of
  Just ty -> return (Node (E <$> HypotheticalTypingJudgement ctx (Var x) ty) [] ,ty)
  Nothing -> Left $ "failed to find a type for " ++  show exp ++ " in context"  




typeCheck ctx (Succ e) = let
  premise = typeCheck ctx $ withoutLoc e 
  in
    do
    (der,ty) <- premise
    if ty == Nat then
      return (Node (E <$> HypotheticalTypingJudgement ctx (Succ e) ty) [der] ,Nat)
    else Left $ "Expecting expression of type Nat but " ++ show e ++ " is of type " ++ show ty




typeCheck ctx Zero = return (Node (E <$> HypotheticalTypingJudgement ctx Zero Nat) [] ,Nat)




typeCheck ctx exp@(Rec_Nat e0 x y e g) = let
  lpremise = typeCheck ctx $ withoutLoc g
  mpremise = typeCheck ctx $ withoutLoc e0
  t = snd <$> mpremise
  h1 = TypingJudgement (Var $ B True) Nat    -- x is represented by True
  h2 = TypingJudgement (Var $ B False) <$> t -- y is represented by False
  rpremise = do
    h2' <- h2
    typeCheck (liftExtend [h1 ,h2'] ctx) $ fromScope $ withoutLoc e    
  in
  do
  rpremise' <- rpremise
  lpremise' <- lpremise
  mpremise' <- mpremise
  t' <- t
  if t' == snd rpremise' then
    return (Node (E <$> HypotheticalTypingJudgement ctx exp t')
                (fmap fst [lpremise',mpremise',rpremise']),t')
  else
    Left $ "type missmatch recurser base case type and body type must be the same \n"
           ++ "e is of type "
           ++ show (snd rpremise') ++ " e0 is of type : " ++ show t




-- infra stuff
data Forall where
  E :: Show a => a -> Forall 
instance Show Forall where
  show (E a) = show a



-- utility functions
  
lookfor :: Eq a => Exp l a -> Ctx l a -> Maybe TExp 
lookfor exp [] = Nothing
lookfor exp ((TypingJudgement e ty):ctx) = if e == exp then Just ty else lookfor exp ctx




liftContext :: Ctx l a -> Ctx l (Bound.Var b a)
liftContext = fmap (fmap F)




liftExtend :: Ctx l (Bound.Var b a) -> Ctx l a -> Ctx l (Bound.Var b a)
liftExtend newjuds ctx =  newjuds ++ liftContext ctx 




