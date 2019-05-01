module LambdaCalcParser where
import LambdaCalcImplementation
import ParserMonad

parser :: Parser Term
parser = undefined


-- ungraded bonus: also parse wild cards like _ so you can write ( \ _ -> x)
-- ungraded bonus: parse numbers into their church encodings

-- for repl testing
data LambdaOut = ParseError | Result Term deriving (Show, Eq)

exec :: String -> LambdaOut
exec s = case (parse parser) s of
  Just (ast,"") -> Result $ eval ast
  _  -> ParseError
