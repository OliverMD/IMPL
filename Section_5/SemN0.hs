module SemN0 where
import AbsRules

-- simplistic implementation of states
data Value = Null | IsBool Bool | IsInt Integer deriving (Eq, Show)
type State = [(String, Value)]
emptyState :: State -- the empty state
emptyState = []
addToState :: String -> State -> State -- put new declaration in state
addToState v s = (v, Null):s
fetch :: State -> String -> Value -- get value associated with a name
fetch s name = head [val | (nm, val) <- s, nm==name]
update :: (String, Value) -> State -> State -- update value associated with a name
-- update (n, v) [] = [(n,v)]
update p@(n, v) (q@(m,_):ss) | n==m      = p:ss
                             | otherwise = q:update p ss
update p@(n,v) (q@(m,_):[]) = [p]

evaluateIntExp :: State -> IntExp -> Value
evaluateIntExp s (IntExpIdent (Ident id)) | Null <- val = error ("Unset variable: " ++ show id)
                                    | otherwise = val
                                                  where val = fetch s id
evaluateIntExp s (IntExpInteger x) = IsInt x
evaluateIntExp s (IAdd x y) = IsInt (x` + y`)
                              where (IsInt x`) = evaluateIntExp s x
                                    (IsInt y`) = evaluateIntExp s y
evaluateIntExp s (IMul x y) = IsInt (x` * y`)
                              where (IsInt x`) = evaluateIntExp s x
                                    (IsInt y`) = evaluateIntExp s y
evaluateIntExp s (INeg x y) = IsInt (x` - y`)
                              where (IsInt x`) = evaluateIntExp s x
                                    (IsInt y`) = evaluateIntExp s y
evaluateBoolExp :: State -> BExp -> Value
evaluateBoolExp s (BExpIdent (Ident x)) | Null <- val = error ("Unset variable: " ++ show x)
                                        | otherwise = val
                                                      where val = fetch s x
evaluateBoolExp s (BExp_TRUE) = IsBool True
evaluateBoolExp s (BExp_FALSE) = IsBool False
evaluateBoolExp s (BExp1 x y) = IsBool (x` || y`)
                                        where (IsBool x`) = evaluateBoolExp s x
                                              (IsBool y`) = evaluateBoolExp s y
evaluateBoolExp s (BExp2 x y) = IsBool (x` == y`)
                                        where (IsBool x`) = evaluateBoolExp s x
                                              (IsBool y`) = evaluateBoolExp s y
evaluateBoolExp s (BAnd x y) = IsBool (x` && y`)
                                       where (IsBool x`) = evaluateBoolExp s x
                                             (IsBool y`) = evaluateBoolExp s y
evaluateBoolExp s (BNot x) = IsBool (not x`)
                                     where (IsBool x`) = evaluateBoolExp s x

meaningN0 :: Stmt -> [Value]
meaningN0 s = out
              where (_, out) = bigstepN0 s emptyState


bigstepN0 :: Stmt -> State -> (State, [Value])
bigstepN0 SSkip s = (s, [])
bigstepN0 (SBAss (Ident s) x) state = (update (s, evaluateBoolExp x) state, [])
bigstepN0 (SIAss (Ident s) x) state = (update (s, evaluateIntExp x) state, [])
bigstepN0 (SPnt x) state = (state, [evaluateIntExp x])
bigstepN0 (STerm a b) state = (t,o ++ u)
                              where (s,o) = bigstepN0 a state
                                    (t,u) = bigstepN0 b s
bigstepN0 (SIF BExp Stmt Stmt)
