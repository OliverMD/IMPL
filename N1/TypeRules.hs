module TypeRules where

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
validIntExp env typ (IntExpPIdent (PIdent all@(_,x))) | envRet == Just typ = noError
                                    | envRet == Nothing = nodecl all
                                    | otherwise = badcontext typ all
                                                  where envRet = typeOfVar env x
validIntExp env typ (IntExpInteger x) = noError
validIntExp env typ (IAdd a b) | typ == TInt = (validIntExp env TInt a) `jErr` (validIntExp env TInt b)
                               | otherwise = errMs (1,1) "IAdd Error"
validIntExp env typ (IMul a b) | typ == TInt = (validIntExp env TInt a) `jErr` (validIntExp env TInt b)
                               | otherwise = errMs (2,2) "IMul Error"
validIntExp env typ (INeg a b) | typ == TInt = (validIntExp env TInt a) `jErr` (validIntExp env TInt b)
                               | otherwise =  errMs (3,3) "INeg Error"

validBoolExp :: Env -> BExp -> ErrorMessage
validBoolExp env (BExpPIdent (PIdent all@(_,x))) | (typeOfVar env x) == Just TBool = noError
                               | (typeOfVar env x) == Nothing = nodecl all
                               | otherwise = badcontext TBool all
validBoolExp env (BExp_TRUE) = noError
validBoolExp env (BExp_FALSE) = noError
validBoolExp env (BExp1 a b) = (validBoolExp env a) `jErr` (validBoolExp env b)
validBoolExp env (BExp2 a b) = (validBoolExp env a) `jErr` (validBoolExp env b)
validBoolExp env (BAnd a b) = (validBoolExp env a) `jErr` (validBoolExp env b)
validBoolExp env (BNot a) = validBoolExp env a

validStm :: Env -> Stmt -> ErrorMessage
validStm env (SSkip) = noError
validStm env (SBAss x bexp) = validBoolExp env bexp
validStm env (SIAss x iexp) = validIntExp env TInt  iexp
validStm env (SPnt iexp) = validIntExp env TInt iexp
validStm env (STerm s1 s2) = (validStm env s1) `jErr` (validStm env s2)
validStm env (SIF bexp s1 s2) = (validBoolExp env bexp) `jErr` (validStm env s1) `jErr` (validStm env s2)
validStm env (SWhile bexp s) = (validBoolExp env bexp) `jErr` (validStm env s)
validStm env (SParen s) = validStm env s
validStm env (SBlock (PIdent (_, x)) typ s) = (validStm (addToEnv (typ, x) env) s)


txStm :: Stmt -> Err Stmt
txStm s | typeErrors == noError = Ok s
        | otherwise = Bad (reportErr typeErrors)
                      where typeErrors = validStm emptyEnv s
