{-# LANGUAGE InstanceSigs #-}
module Program(T, parse, fromString, toString, exec) where

import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
import qualified GHC.Conc as Statement

newtype T = Program [Statement.T] -- to be defined

instance Eq T where
  p1 == p2 = show p1 == show p2

instance Show T where
  show = toString

instance Parse T where
  parse :: Parser T
  parse = iter Statement.parse >-> \stmts -> Program stmts
  toString :: T -> String
  toString (Program stmts) = unlines (map Statement.toString stmts)

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.execute stmts Dictionary.empty
