-- Completed by: Chonratid Pangdee, chopan-7@student.ltu.se
module Program(T, parse, fromString, toString, exec) where
import qualified Dictionary
import           Parser     hiding (T)
import           Prelude    hiding (fail, return)
import qualified Statement
newtype T = Program [Statement.T] -- Task 4

instance Parse T where
  parse = (iter Statement.parse) >-> Program	-- iterate through list of statements and run as program

  -- iterate through statements and run toString
  toString (Program (x:xs)) = (Statement.toString x) ++ toString (Program xs)
  toString (Program [])     = ""

-- Program execute: call execute in Statement-module
exec (Program stmts) lst = Statement.exec stmts Dictionary.empty lst
