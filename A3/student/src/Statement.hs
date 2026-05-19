{-# LANGUAGE InstanceSigs #-}
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
    Skip |
    Comment String |
    Read String |
    Write Expr.T |
    Begin [Statement] 
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

if' :: Parser Statement
if' = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> \((condition, thenA), elseA) -> If condition thenA elseA


while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> \(condition, thenA) -> While condition thenA

skip :: Parser Statement
skip = accept "skip" # require ";" >-> \_ -> Skip

comment :: Parser Statement
comment = Parser.comment >-> \x -> Comment x

read :: Parser Statement 
read = accept "read" -# word #- require ";" >-> \x -> Read x

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> \x -> Write x

begin :: Parser Statement
begin = accept "begin" -# iter parse #- require "end" >-> \x -> Begin x

class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where
    -- execute :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]
    
    
    execute :: [Statement]-> Dictionary.T String Integer -> [Integer] -> [Integer]
    execute [] _ _ = [] 

    execute (If cond thenStmts elseStmts: stmts) dict input =
        case Expr.value cond dict of
            Left err -> error err
            Right v ->
                if v > 0 then
                    execute (thenStmts: stmts) dict input
                else
                    execute (elseStmts: stmts) dict input

    execute (Assignment variable expression: stmts) dict input = 
        case Expr.value expression dict of
            Left err -> error err 
            Right v -> execute stmts (Dictionary.insert (variable, v) dict) input 

    execute (While cond thenStmts: stmts) dict input = 
        case Expr.value cond dict of 
            Left err -> error err
            Right v -> 
                if v > 0 then 
                    execute (thenStmts : While cond thenStmts : stmts) dict input
                else 
                    execute stmts dict input

    execute (Skip: stmts) dict input = execute stmts dict input 

    execute (Comment _: stmts) dict input = execute stmts dict input 

    execute (Read variable: stmts) dict input = 
        case input of 
            [] -> execute stmts dict input 
            x:xs -> execute stmts (Dictionary.insert (variable, x) dict) xs

    execute (Write expression: stmts) dict input = 
        case Expr.value expression dict of 
            Left err -> error err
            Right v ->
                v: execute stmts dict input 

    execute (Begin statments: stmts) dict input = execute (statments ++ stmts) dict input 
    
        
instance Parse Statement where

  parse = begin ! if' ! while ! skip ! Statement.read ! write ! Statement.comment ! assignment
  toString :: Statement -> String
  toString (While expression statment) = "while " ++ Expr.toString expression ++ " do " ++ toString statment
  toString (If cond thenA thenB) = 
    "if " ++ Expr.toString cond ++ " then\n" ++ "\t" ++ toString thenA ++ "\nelse\n\t" ++  toString thenB ++ "\n"
  toString Skip = "skip;"
  toString (Comment text) = "--" ++ text ++ "\n"
  toString (Read variable) = "read " ++ variable  ++ ";"
  toString (Write expression) = "write " ++ Expr.toString expression ++ ";"
  toString (Begin statments) = "begin\n" ++ unlines (map (\x -> "\t" ++ toString x) statments) ++ "end\n"
  toString (Assignment variable expression) = variable ++ " := " ++ Expr.toString expression ++ ";"


