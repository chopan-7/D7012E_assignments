-- Completed by: Chonratid Pangdee, chopan-7@student.ltu.se
module Statement(T, parse, toString, fromString, exec) where
import qualified Dictionary
import qualified Expr
import           Parser     hiding (T)
import           Prelude    hiding (fail, repeat, return)
type T = Statement

-- Task 3a
data Statement = Assignment String Expr.T
 | If Expr.T Statement Statement
 | Skip
 | Begin [Statement]
 | While Expr.T Statement
 | Read String
 | Write Expr.T
 | Repeat Statement Expr.T

 deriving Show

-- Task 3b
assignment = (word #- accept ":=") # (Expr.parse #- require ";") >-> buildAss
buildAss (v, e) = Assignment v e

ifstmt = accept "if" -# (Expr.parse #- require "then" # parse) # (require "else" -# parse) >-> buildif
buildif ((s1, t1), e1) = If s1 t1 e1

skip = accept "skip" # require ";" >-> buildskip
buildskip _ = Skip

begin = accept "begin" -# iter (parse) #- require "end" >-> buildbegin
buildbegin statements = Begin statements

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildwhile
buildwhile (stmt, doWork) = While stmt doWork

readstmt = accept "read" -# word #- require ";" >-> buildread
buildread str = Read str

write = accept "write" -# Expr.parse #- require ";" >-> buildwrite
buildwrite e = Write e

-- Assignment 2
repeat = accept "repeat" -# Expr.parse #- require "until" # parse #- require ";" >-> buildrepeat
buildrepeat (stmts, cond) = Repeat stmts cond

-- Task 3d
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

-- Execute assingment: a variable gets assigned to a value and inserted to a dictionary in (str, int)-pair.
exec (Assignment v e : stmts) dict input =
    exec stmts (Dictionary.insert (v, (Expr.value e dict)) dict) input

-- Execute skipe
exec (Skip : stmts) dict input =
    exec stmts dict input

-- Execute begin: execute all statements in the input list
exec (Begin stmnt : stmts) dict input =
    exec (stmnt++stmts) dict input

-- Execute while: if statement is true, recursive call the while and execute doWork. Else, stop.
exec (While stmt doWork: stmts) dict input =
    if (Expr.value stmt dict)>0
    then exec (doWork:(While stmt doWork):stmts) dict input
    else exec stmts dict input

-- Execute read: save the first input as variable and execute the rest if the statements.
exec (Read str:stmts) dict input =
    exec stmts (Dictionary.insert (str, head input) dict) (tail input)

-- Execute write: save new values in input-list
exec (Write cond: stmts) dict input =
    (Expr.value cond dict) : (exec stmts dict input)

-- Execute repeat: statements once if condition is true. Else, execute until statement is true
exec (Repeat stmt cond: stmts) dict input =
    exec (stmt : (If cond Skip (Repeat stmt cond): stmts)) dict input

instance Parse Statement where
-- Task 3c
  parse = assignment ! ifstmt ! skip ! begin ! while ! readstmt ! write ! repeat

-- Task 5
  toString (If cond thenStmts elseStmts) = "if " ++ (Expr.toString cond) ++ " then\n      " ++ (toString thenStmts) ++ "    else\n      " ++ (toString elseStmts)
  toString (Assignment v e) = v ++ " := " ++ (Expr.toString e) ++ ";\n"
  toString (Skip) = "skip;\n"
  toString (Begin stmts) = "begin\n" ++ (stmtToString stmts)
   where stmtToString (stmt:stmts) = "    " ++ (Expr.toString stmt) ++ (stmtToString stmts)
         stmtToString [] = "  end\n"

  toString (While stmt doWork) = "while " ++ (Expr.toString stmt) ++ " do\n  " ++ toString doWork
  toString (Read str) = "read " ++ str ++ ";\n"
  toString (Write cond) = "write" ++ (Expr.toString cond) ++ ";\n"
  toString _ = ""
