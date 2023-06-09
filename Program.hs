--Christoffer Lindkvist
module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show -- to be defined

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program s) = concatMap Statement.toString s

exec :: T -> [Integer] -> [Integer]
exec (Program s) arr = Statement.exec s Dictionary.empty arr  -- we start with an empty environment