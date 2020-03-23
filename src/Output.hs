{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Output where
import DataTypes
import TypeChecker
import Text.Parsec
import Data.Tree
import Bound 

data OutputOpts = OutputOpts SourceLocInfoLevel VariableEncoding
data SourceLocInfoLevel = TopLevelExp | None 
data VariableEncoding = DeBrujin | OriginalName

-- loc info is not showed
outputExp :: OutputableVar a => VariableEncoding -> Exp l a -> String
outputExp DeBrujin = outputExpDeBrujin
outputExp OriginalName = outputExpOriginalNames . fmap outputVar

outputExpDeBrujin :: OutputableVar a => Exp l a -> String
outputExpDeBrujin e = case e of 
  Var x -> shrink $ outputVar x 
  
  App (_,f) (_,x) -> "(" ++ outputExpDeBrujin f ++ " " ++ outputExpDeBrujin x ++ ")"
  
  Lam ty varName (l,body) -> "(λ" ++ varName ++ " : " ++ outputTExp ty ++  ". " 
    ++ outputScopeDeBrujin body  ++ ")"
  
  Zero -> "Zero"
  
  Succ (_,x) -> "(Succ" ++ outputExpDeBrujin x ++ ")"  
  
  Rec_Nat (_,m) pName fpName (_,body) (_, val) -> 
    "(Rec_Nat "  
    ++ "(" ++ pName ++ " " ++ fpName ++ ". " ++ outputScopeDeBrujin body ++ ") " 
    ++ outputExpDeBrujin val ++ ")"
  
  where 
    shrink x = let (fs,tail) = span (== 'F') x in show (length fs) ++ "F" ++ tail

outputScopeDeBrujin :: (OutputableVar a, OutputableVar b) => Scope b (Exp l) a -> String
outputScopeDeBrujin s = outputExpDeBrujin $ fromScope s 


outputExpOriginalNames :: Exp l Name -> String
outputExpOriginalNames e = case e of 
  Var x -> outputVar x 
  
  App (lf,f) (lx,x) -> "(" ++ outputExpOriginalNames f ++ " " ++ outputExpOriginalNames x ++ ")"
  
  Lam ty varName (l,body) -> "(λ" ++ varName ++ " : " ++ outputTExp ty ++  ". " 
    ++ outputScopeOriginalNames body (\() -> varName) ++ ")"
  
  Zero -> "Zero"
  
  Succ (_,x) -> "(Succ" ++ outputExpOriginalNames x ++ ")"  
  
  Rec_Nat (_,m) pName fpName (_,body) (_, val) ->
    let binderMap True = pName
        binderMap False = fpName
    in
    "(Rec_Nat "  
    ++ "(" ++ pName ++ " " ++ fpName ++ ". " ++ outputScopeOriginalNames body binderMap ++ ") " 
    ++ outputExpOriginalNames val ++ ")"

outputScopeOriginalNames ::  Scope b (Exp l) Name -> (b -> Name) -> String
outputScopeOriginalNames s originalNames = outputExpOriginalNames $ instantiate (\b -> pure $ originalNames b) s   


outputTExp :: TExp -> String
outputTExp = show


class OutputableVar a where
    outputVar :: a -> String

instance (OutputableVar a, OutputableVar b) => OutputableVar (Var a b) where
    outputVar (B x) = "B" ++ outputVar x  
    outputVar (F x) = "F" ++ outputVar x

instance OutputableVar Name where
    outputVar x = x

instance OutputableVar () where
    outputVar = show

instance OutputableVar Bool where
    outputVar = show


--pretty printing functions

ppHypotheticalJudgement :: (Show l,Show a) => HypotheticalTypingJudgement l a -> String
ppHypotheticalJudgement (HypotheticalTypingJudgement ctx exp ty) =
  let
   prettyCtx = foldl f "" (fmap ppTypingJudgement ctx)
   f str tyStr = str ++ " " ++ tyStr
  in
    prettyCtx ++ " ⊢ " ++ ppTypingJudgement (TypingJudgement exp ty)


ppTypingJudgement :: (Show l,Show a) => TypingJudgement l a -> String 
ppTypingJudgement (TypingJudgement exp ty) =  show exp ++ " : " ++ show ty

outputTypingJudgement :: (Show l, OutputableVar a) => OutputOpts -> TypingJudgement l a -> String 
outputTypingJudgement (OutputOpts locLevel enc) (TypingJudgement exp ty) = outputExp enc exp ++ " : " ++ outputTExp ty


ppDerivation :: Show l => Derivation l -> String
ppDerivation der = drawTree $ fmap ppHypotheticalJudgement der 




pprint :: Show l => Either TypeError (Derivation l,TExp) -> String
pprint (Left typeError) = typeError
pprint (Right (der,ty)) =
  "Expression is of type :" ++ show ty ++ "\n" ++ ppDerivation der



-- veryy badd veryy badd !!!
pprint' :: Show l => Either ParseError (Either TypeError (Derivation l, TExp)) -> String
pprint' (Left pe) = show pe
pprint' (Right thing) = pprint thing


