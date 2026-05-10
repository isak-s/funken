module Program(T, parse, fromString, toString, exec) where

import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Eq T where
  p1 == p2 = False -- FIXME

instance Show T where
  show = toString

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) = concatMap (\s -> toString s ++ "\n") stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.execute stmts Dictionary.empty
