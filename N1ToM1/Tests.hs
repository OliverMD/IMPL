module Tests where

import ParRules
import TypeRules
import ErrM
import AbsRules

getTree :: Err Stmt -> Stmt
getTree (Ok a) = a
getTree (Bad a) = error a

testString :: String -> Err Stmt
testString s = txStm (getTree (pStmt (myLexer s)))

simpleTestString :: String
simpleTestString = "[x: Int @ x := 8 + 4]"
