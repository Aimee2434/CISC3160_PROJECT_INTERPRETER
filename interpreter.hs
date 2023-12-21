import Data.Char
import Data.List
import Data.Maybe
import Data.Map.Strict (Map, empty, insert, lookup, member)
import Text.Parsec
import Text.Parsec.String
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isAlpha, isAlphaNum)

-- AST
data Exp = Add Exp Term | Sub Exp Term | Term deriving (Show)
data Term = Mul Term Fact | Fact deriving (Show)
data Fact = Par Exp | Neg Fact | Pos Fact | Lit Integer | Var String deriving (Show)
data Stmt = Assign String Exp deriving (Show)

-- Keep track
type State = Map String Integer

-- Token type
data Token
  = TokIdentifier String
  | TokLiteral Integer
  | TokOperator String
  | TokOpenParen
  | TokCloseParen
  | TokSemicolon
  | TokEquals
  deriving (Show, Eq)

-- Tokenizer
lexer :: Parser [Token]
lexer = spaces >> many (lexeme token)

-- Whitespace after lexeme
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- Token parsers
identifier :: Parser Token
identifier = TokIdentifier <$> (:) <$> letter <*> many (alphaNum <|> char '_')

literal :: Parser Token
literal = TokLiteral <$> read <$> many1 digit

operator :: Parser Token
operator = TokOperator <$> oneOf "+-*/"

openParen :: Parser Token
openParen = TokOpenParen <$ char '('

closeParen :: Parser Token
closeParen = TokCloseParen <$ char ')'

semicolon :: Parser Token
semicolon = TokSemicolon <$ char ';'

equals :: Parser Token
equals = TokEquals <$ char '='

token :: Parser Token -- Combined token parser
token = identifier <|> literal <|> operator <|> openParen <|> closeParen <|> semicolon <|> equals

-- Function
interpret :: String -> Either String State
interpret input = case parse lexer "" input of
  Left err -> Left $ "Syntax error: " ++ show err
  Right tokens -> case parseProgram (tokensToAssignments tokens) of
    Left err -> Left err
    Right stmts -> case evalStmts stmts Map.empty of
      Left err -> Left err
      Right finalState -> Right finalState

-- Convert tokens to assignments
tokensToAssignments :: [Token] -> [(String, Exp)]
tokensToAssignments tokens = case runParser assignmentList () "" tokens of
  Left err -> error $ "Token conversion error: " ++ show err
  Right result -> result

assignmentList :: Parser [(String, Exp)]
assignmentList = sepBy assignment semicolon

assignment :: Parser (String, Exp)
assignment = do
  var <- identifier
  equals
  exp <- expression
  return (var, exp)

expression :: Parser Exp
expression = chainl1 term addOp

term :: Parser Term
term = chainl1 fact mulOp

fact :: Parser Fact
fact =
  try (Par <$> (openParen *> expression <* closeParen))
    <|> try (Neg <$> (char '-' *> fact))
    <|> try (Pos <$> (char '+' *> fact))
    <|> Lit <$> literal
    <|> Var <$> identifier

addOp, mulOp :: Parser (Exp -> Exp -> Exp)
addOp = (Add <$ char '+') <|> (Sub <$ char '-')
mulOp = Mul <$ char '*'

-- Evaluate
evalStmts :: [Stmt] -> State -> Either String State
evalStmts [] state = Right state
evalStmts (stmt : stmts) state = case evalStmt stmt state of
  Left err -> Left err
  Right newState -> evalStmts stmts newState

evalStmt :: Stmt -> State -> Either String State
evalStmt (Assign var exp) state =
  case evalExp exp state of
    Left err -> Left err
    Right val -> Right $ Map.insert var val state

evalExp :: Exp -> State -> Either String Integer
evalExp (Add e t) state = (+) <$> evalExp e state <*> evalTerm t state
evalExp (Sub e t) state = (-) <$> evalExp e state <*> evalTerm t state
evalExp (Term) state = evalTerm Term state

evalTerm :: Term -> State -> Either String Integer
evalTerm (Mul t f) state = (*) <$> evalTerm t state <*> evalFact f state
evalTerm (Fact) state = evalFact Fact state

evalFact :: Fact -> State -> Either String Integer
evalFact (Par e) state = evalExp e state
evalFact (Neg f) state = negate <$> evalFact f state
evalFact (Pos f) state = evalFact f state
evalFact (Lit n) _ = Right n
evalFact (Var var) state =
  case Map.lookup var state of
    Just val -> Right val
    Nothing -> Left $ "Error: Variable '" ++ var ++ "' not initialized."


--Test
--in: x=001; out: error
--in: x_2=0; out: error
--in: x = 0 y = x; z = ---(x+y); out: error
--in: x = 1; y = 2; z = ---(x+y)*(x+-y); out: x=1 y=2 z=3
--in: x_2=0; out: 0 (changed ValidIdentifier function)
--in: x = 0 y = x; z = ---(x+y); out: xyz=0 (after change for x_2=0)
--after a few changes, the code is not working anymore. Stay the previous code for xyz=0
