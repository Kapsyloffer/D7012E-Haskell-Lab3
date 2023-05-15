module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Assignment String Expr.T 
    | If Expr.T Statement Statement --if statement
    | Read String -- Read a string as input. 
    | Repeat Statement Expr.T -- repeatear en statement until...
    | Seq [Statement] -- A sequence of statements.
    | Skip --nop
    | While Expr.T Statement --while loop if true
    | Write Expr.T --Write
    deriving Show
    
assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> build
    where build :: (String, Expr.T) -> Statement 
          build (v, e) = Assignment v e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict) > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

parse_if :: Parser Statement --parsea en if then else statement
parse_if = accept "if" -# Expr.parse #- require "then" # parse #- accept "else" # parse  >-> build
    where build :: ((Expr.T, Statement), Statement) -> Statement
          build ((c, t), e) = If c t e

parse_read :: Parser Statement --parsear en read statement
parse_read = accept "read" -# word #- require ";" >-> build
    where build :: String -> Statement
          build s = Read s

parse_repeat :: Parser Statement --parsear en repeat
parse_repeat = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> build
    where build :: (Statement, Expr.T) -> Statement
          build (do_, cond) = Repeat do_ cond

parse_seq ::  Parser Statement --parserar en sequence av statements
parse_seq = accept "begin" -# iter parse #- require "end" >-> build
    where build :: [Statement] -> Statement
          build = Seq 
          
parse_skip :: Parser Statement
parse_skip = accept "skip" # require ";" >-> build
    where build :: a -> Statement
          build _ = Skip

parse_while :: Parser Statement --parsear en while loop
parse_while = accept "while" -# Expr.parse #- require "do" # parse >-> build
    where build :: (Expr.T, Statement) -> Statement
          build (c, d) = While c d

parse_write :: Parser Statement --parsera en write statement
parse_write = accept "write" -# Expr.parse #- require ";" >-> build
    where build :: Expr.T -> Statement
          build = Write

instance Parse Statement where
    parse = assignment ! parse_if ! parse_read ! parse_repeat ! parse_seq ! parse_skip ! parse_while ! parse_write
    toString (Assignment var val) = var ++ ":=" ++ Expr.toString val ++ ";\n" 
    toString (If cond thenStmts elseStmts) = "if" ++ Expr.toString cond ++ "then\n" ++ toString thenStmts  ++ "else\n" ++ toString elseStmts
    toString (Read s) = "read" ++ s ++ ";\n"
    toString (Repeat do_ cond) = "repeat\n" ++ toString do_ ++ "until" ++ Expr.toString cond ++ "\n"
    toString (Seq stmts) = "seq\n" ++ concatMap toString stmts ++ "end\n"
    toString (Skip) = "skip;\n"
    toString (While cond do_) = "while" ++ Expr.toString cond ++ "do\n" ++ toString do_
    toString (Write e) = "write" ++ Expr.toString e ++ ";\n"
