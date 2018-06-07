module TypeChecker where

import DataTypes
import Data.Tree
import Control.Lens  
import Text.Parsec
data TypingJudgement  = TypingJudgement Exp TExp  deriving Show 

data HypotheticalTypingJudgement = HypotheticalTypingJudgement  [TypingJudgement] Exp TExp deriving Show



type Derivation = Tree HypotheticalTypingJudgement

type TypeError = String 

type Ctx = [TypingJudgement]

_der = _Right . _1
_ty = _Right . _2




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


typeCheck ::Ctx -> Exp -> Either TypeError (Derivation,TExp)
typeCheck ctx (Lam (x , v) body) = let
  jud = TypingJudgement (Var x) v
  premise =  typeCheck (extendContext jud ctx) body
  cons = do
    u <- fmap snd  premise
    return $ HypotheticalTypingJudgement ctx (Lam (x , v) body) (Arr v u)
  in
    do
      u <- fmap snd premise
      cons' <- cons
      (over _der (\d -> Node cons' [d]) premise) & _ty .~ (Arr v u) 
    

typeCheck ctx (LApp f g) = let
  lpremise = typeCheck ctx f
  rpremise = typeCheck ctx g
  fCodomain = do
    g_ty <- fmap snd rpremise
    f_ty <- fmap snd lpremise
    case f_ty of
             Arr s t -> if s == g_ty then return $ t
               else Left $ "then function expects arg of type" ++ show s
               ++ "but it has been given a value of type " ++ show g_ty 
             _ -> Left $ show f ++ "is not of function type"

  cons = do
    t <- fCodomain
    return $ HypotheticalTypingJudgement ctx (LApp f g) t
    
  in 
    do
    cons' <- cons
    lpremise' <- lpremise
    rpremise' <- rpremise
    fCodomain' <- fCodomain
    return $ (Node cons' [fst lpremise' , fst rpremise'], fCodomain') 

typeCheck ctx (Var x)  = case lookfor (Var x) ctx of
  Just (TypingJudgement e ty) ->
    return (Node (HypotheticalTypingJudgement ctx e ty) [] ,ty)
  Nothing ->
    Left $ "failed to find type for " ++  show (Var x) ++ " in context"  

typeCheck ctx (Succ e) = let
  premise = typeCheck ctx e 
  in
    do
      premise' <- premise
      case premise' of
        (der, ty) | ty == Nat ->
           return $ (Node (HypotheticalTypingJudgement ctx (Succ e) ty) [der] ,Nat)
        (_, ty') ->  Left $ "Expecting expression of type Nat but " ++ show e ++ " of type " ++ show ty'


typeCheck ctx Zero = return $ (Node (HypotheticalTypingJudgement ctx Zero Nat) [] ,Nat)




  

lookfor :: Exp -> [TypingJudgement]-> Maybe TypingJudgement
lookfor exp [] = Nothing
lookfor exp ((TypingJudgement e ty):ctx) = if e == exp then Just cons else lookfor exp ctx
  where
    cons = TypingJudgement  e ty

 


ppHypotheticalJudgement :: HypotheticalTypingJudgement -> String
ppHypotheticalJudgement (HypotheticalTypingJudgement ctx exp ty) =
  let
   prettyCtx = foldl f "" (fmap ppTypingJudgement ctx)
   f str tyStr = str ++ " " ++ tyStr
  in
    prettyCtx ++ " ⊢ " ++ ppTypingJudgement (TypingJudgement exp ty)


extendContext :: TypingJudgement -> [TypingJudgement] -> [TypingJudgement]
extendContext jud@(TypingJudgement e _ ) ctx = jud:(filter p ctx)
  where p (TypingJudgement e' _) = if e' == e then False else True 




--pretty printing functions

ppTypingJudgement :: TypingJudgement -> String 
ppTypingJudgement (TypingJudgement exp ty) =  show exp ++ " : " ++ show ty


ppDerivation :: Derivation -> String
ppDerivation der = drawTree $ fmap ppHypotheticalJudgement der 

pprint :: Either TypeError (Derivation,TExp) -> String
pprint (Left typeError) = typeError
pprint (Right (der,ty)) =
  "Expression is of type :" ++ show ty ++ "\n" ++ ppDerivation der

-- veryy badd veryy badd !!!
pprint' :: Either ParseError (Either TypeError (Derivation, TExp)) -> String
pprint' (Left pe) = show pe
pprint' (Right thing) = pprint thing
