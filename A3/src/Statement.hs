module Statement(T, parse, toString, fromString, execute) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Begin [Statement] |
    Read String |
    Write Expr.T |
    Skip
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

skip :: Parser Statement
skip = accept "skip" #- require ";" >-> const Skip

read_ :: Parser Statement
read_ = accept "read" -# word #- require ";" >-> Read

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> Write

while :: Parser Statement
while = accept "while" -# (Expr.parse #- require "do") # parse  >-> uncurry While

if_ :: Parser Statement
if_ = accept "if" -# (Expr.parse #- require "then") # (parse #- require "else") # parse >-> (\((expr, stmt1), stmt2) -> If expr stmt1 stmt2)

-- find begin, create statements from each line. return Begin [list of statements]
begin :: Parser Statement
begin = accept "begin" -# (iter parse) #- require "end" >-> Begin


class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where

    execute [] _ _ = []

    -- execute :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]
    execute (If cond thenStmts elseStmts: stmts) dict input =
        case Expr.value cond dict of
            Left err_ -> error err_
            Right v ->
                if v > 0 then
                    execute (thenStmts: stmts) dict input
                else
                    execute (elseStmts: stmts) dict input
    execute (Assignment name expr : stmts) dict input =
        case Expr.value expr dict of
            Left err_ -> error err_
            Right v -> execute stmts newDict input
                where newDict = Dictionary.insert (name, v) dict

    execute (Skip:stmts) dict input = execute stmts dict input

    execute (Read _ : _) _ [] = error "no input"
    execute (Read name : stmts) dict (curr:input) = execute stmts newDict input
        where newDict = Dictionary.insert (name, curr) dict

    execute (Write e:stmts) dict input =
        case Expr.value e dict of
            Left err_ -> error err_
            Right v -> v : execute stmts dict input

    execute (While cond stmt : stmts) dict input =
        case Expr.value cond dict of
            Left err_ -> error err_
            Right v ->
                if v > 0 then
                    execute (stmt : While cond stmt:stmts) dict input
                else execute stmts dict input

    -- variables defined in the begin will not be visible outside. Scope!
    execute (Begin [] : outside) dict input = execute outside dict input
    execute (Begin inside : outside) dict input =
        execute inside dict input ++ execute outside dict input

instance Parse Statement where
  parse = assignment ! skip ! read_ ! write ! while ! if_ ! begin
  toString Skip = "skip;"
  toString (Assignment name expr) = name ++ " := " ++ Expr.toString expr ++ ";"
  toString (If cond stmt1 stmt2) = "if " ++ Expr.toString cond ++ "\nthen\n" ++ toString stmt1 ++ "\nelse\n" ++ toString stmt2
  toString (While cond stmt) = "while " ++ Expr.toString cond ++ " do " ++ toString stmt
  toString (Begin stmts) = "begin\n" ++ concatMap toString stmts ++ "end\n"
  toString (Read str) = "read " ++ str ++ ";"
  toString (Write expr) = "write " ++ Expr.toString expr ++ ";"
