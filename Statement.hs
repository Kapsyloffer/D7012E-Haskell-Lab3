module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T 
    | If Expr.T Statement Statement --if statement
    | Print Expr.T --Output a string.
    | Read String -- Read a string as input.
    | Seq [Statement] -- A sequence of statements.
    | Skip --nop
    | While Expr.T Statement --while loop if true
    deriving Show
    
assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss :: (String, Expr.t) -> Statement
buildAss (v, e) = Assignment v e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
