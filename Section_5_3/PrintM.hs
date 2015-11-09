{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintM where

-- pretty-printer generated by the BNF converter

import AbsM
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)




instance Print Line where
  prt i e = case e of
    Cmd strs instruction -> prPrec i 0 (concatD [prt 0 strs, doc (showString ": "), prt 0 instruction])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "\n"), prt 0 xs])
instance Print Instruction where
  prt i e = case e of
    Nop -> prPrec i 0 (concatD [doc (showString "nop")])
    Hlt -> prPrec i 0 (concatD [doc (showString " hlt ")])
    Lim str n -> prPrec i 0 (concatD [doc (showString "lim"), prt 0 str, prt 0 n])
    Mov str1 str2 -> prPrec i 0 (concatD [doc (showString "mov"), prt 0 str1, prt 0 str2])
    Add str1 str2 str3 -> prPrec i 0 (concatD [doc (showString "add"), prt 0 str1, prt 0 str2, prt 0 str3])
    Mul str1 str2 str3 -> prPrec i 0 (concatD [doc (showString "mul"), prt 0 str1, prt 0 str2, prt 0 str3])
    Neg str1 str2 -> prPrec i 0 (concatD [doc (showString "neg"), prt 0 str1, prt 0 str2])
    Prn str -> prPrec i 0 (concatD [doc (showString "prn"), prt 0 str])
    Jmp str -> prPrec i 0 (concatD [doc (showString "jmp"), prt 0 str])
    Beq str1 str2 str3 -> prPrec i 0 (concatD [doc (showString "beq"), prt 0 str1, prt 0 str2, prt 0 str3])
    Blt str1 str2 str3 -> prPrec i 0 (concatD [doc (showString " blt "), prt 0 str1, prt 0 str2, prt 0 str3])
    Lor str1 str2 str3 -> prPrec i 0 (concatD [doc (showString " lor "), prt 0 str1, prt 0 str2, prt 0 str3])


