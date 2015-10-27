module TypN1 where

import AbsRules
import ErrM

-----------------------------------------------------------------------
-- Environments

type Env = [(Type, String)]
-- a (simplistic) structure to store in-scope variables and their type
-- the last value entered, if any, is always the correct one
emptyEnv :: Env
emptyEnv = []
addToEnv :: (Type, String) -> Env -> Env -- constant
addToEnv = (:)
typeOfVar :: Env -> String -> Maybe Type -- linear
typeOfVar env s | candidates /= [] = Just (head candidates)
                | otherwise        = Nothing
  where candidates = [t | (t,n)<-env, n==s]

-------------------------------------------------------------------------
-- Error messages
       
type ErrorMessage = [String]

jErr :: ErrorMessage -> ErrorMessage -> ErrorMessage -- join two messages
jErr = (++)
noError :: ErrorMessage -- everything is fine!
noError = []
joinErr :: [ErrorMessage] -> ErrorMessage
joinErr = foldr jErr noError
errMs :: (Int, Int) -> String -> ErrorMessage -- make atomic message
errMs p s = [show p ++ ": "++s]
reportErr :: ErrorMessage -> String
reportErr = ('\n':) . unlines
-- particular atomic messages
nodecl :: ((Int, Int), String) -> ErrorMessage
nodecl (p, s) = errMs p ("No declaration for "++show s)
badcontext :: Type -> ((Int, Int), String) -> ErrorMessage
badcontext t (p, s) = errMs p (show s++" appears in "++show t++" context")
unbalancedAssignment, duplicateTargets :: (Int, Int) -> ErrorMessage
unbalancedAssignment p = errMs p "numbers of variables and expressions differ"
duplicateTargets p = errMs p "duplicate variable names"


validIntExp :: Env -> Type -> IntExp -> ErrorMessage
validIntExp env typ (IntExpIdent x) | envRet == Just typ = noError
                                    | envRet == Nothing = nodecl x
                                    | otherwise = badcontext typ x
                                                  where envret = typeOfVar env x
validIntExp env typ (IntExpInteger x) = noError
validIntExp env typ (IAdd a b) | typ == TInt = (validIntExp TInt a) `jErr` (validIntExp TInt b)
                               | otherwise = errMs (1,1) "IAdd Error"
validIntExp env typ (IMul a b) | typ == TInt = (validIntExp TInt a) `jErr` (validIntExp TInt b)
                               | otherwise = errMs (2,2) "IMul Error"
validIntExp env typ (INeg a b) | typ == TInt = (validIntExp TInt a) `jErr` (validIntExp TInt b)
                               | otherwise =  errMs (3,3) "INeg Error"

validBoolExp :: Env -> BExp -> ErrorMessage
validBoolExp env (BExpIdent x) | (typeOfVar env x) == Just TBool = noError
                               | (typeOfVar env x) == Nothing = nodecl x
                               | otherwise = badcontext TBool x
validBoolExp env (BExp_TRUE) = noError
validBoolExp env (BExp_FALSE) = noError
validBoolExp env (BExp1 a b) = (validBoolExp env a) `jErr` (validBoolExp env b)
validBoolExp env (BExp2 a b) = (validBoolExp env a) `jErr` (validBoolExp env b)
validBoolExp env (BAnd a b) = (validBoolExp env a) `jErr` (validBoolExp env b)
validBoolExp env (BNot a) = validBoolExp env a

validStm :: Env -> Stmt -> ErrorMessage
validStm env (SSkip) = noError
validStm env (SBass x bexp) 
