import Text.Parsec
import Text.Parsec.String (Parser)

data Val = 
    Atom String
  | Number Integer
  | List [Val]
  | DottedPair Val Val
  deriving (Show, Eq)

type Variables = [(String, Val)]  -- переменные

-- ============================================
-- Парсер
-- ============================================

whiteSpace :: Parser ()
whiteSpace = skipMany space

atomParser :: Parser Val
atomParser = do
  first <- letter <|> oneOf "+-*/=<>"
  rest <- many (alphaNum <|> oneOf "-_?!")
  return $ Atom (first:rest)

numberParser :: Parser Val
numberParser = do
  sign <- optionMaybe (try (char '-' <* lookAhead digit))
  digits <- many1 digit
  let num = read digits :: Integer
  return $ Number $ case sign of
    Just _  -> -num
    Nothing -> num

emptyListParser :: Parser Val
emptyListParser = do
  char '('
  whiteSpace
  char ')'
  return (List [])

dottedPairParser :: Parser Val
dottedPairParser = do
  char '('
  whiteSpace
  first <- parseExpr
  whiteSpace
  char '.'
  whiteSpace
  second <- parseExpr
  whiteSpace
  char ')'
  return (DottedPair first second)

-- (>>) :: Parser a -> Parser b -> Parser b
regularListParser :: Parser Val
regularListParser = do
  char '('
  whiteSpace
  first <- parseExpr
  whiteSpace
  rest <- many (whiteSpace >> parseExpr)
  whiteSpace
  char ')'
  return (List (first : rest))

listParser :: Parser Val
listParser = try emptyListParser <|> try dottedPairParser <|> regularListParser

quotedParser :: Parser Val
quotedParser = do
  char '\''
  expr <- parseExpr
  return (List [Atom "quote", expr])

parseExpr :: Parser Val
parseExpr = whiteSpace >> (numberParser <|> atomParser <|> listParser <|> quotedParser)

parseLisp :: String -> Either ParseError Val
parseLisp input = parse (parseExpr <* eof) "" input

-- ============================================
-- get / set переменной
-- ============================================
isForbiddenName :: String -> Bool
isForbiddenName "t"   = True
isForbiddenName "nil" = True
isForbiddenName "T"   = True
isForbiddenName "NIL" = True
isForbiddenName _     = False

getVar :: String -> Variables -> Maybe Val
getVar name [] = Nothing
getVar name ((n, v):rest) = 
  if name == n then Just v else getVar name rest

setVar :: String -> Val -> Variables -> Either String Variables
setVar name val variables = 
  if isForbiddenName name
    then Left $ "Cannot redefine built-in constant: " ++ name
    else Right $ (name, val) : variables

-- ============================================
-- Интерпретатор
-- ============================================

toNumber :: Val -> Either String Integer
toNumber (Number n) = Right n
toNumber _ = Left "Expected a number"

toBool :: Val -> Bool
toBool (List []) = False
toBool (Atom "nil") = False
toBool (Atom "t") = True
toBool _ = True

-- eval
eval :: Variables -> Val -> Either String (Val, Variables)

-- Числа
eval variables (Number n) = Right (Number n, variables)

-- Переменные
eval variables (Atom name) = case getVar name variables of
  Just val -> Right (val, variables)
  Nothing  -> Left $ "Undefined variable: " ++ name

-- QUOTE
-- возвращает выражение без вычисления
eval variables (List [Atom "quote", expr]) = Right (expr, variables)

-- IF
eval variables (List [Atom "if", cond, thenExpr, elseExpr]) =
  case eval variables cond of
    Right (condVal, env1) ->
      if toBool condVal
        then eval env1 thenExpr
        else eval env1 elseExpr
    Left err -> Left err

-- CAR
-- первый элемент списка
eval variables (List [Atom "car", arg]) =
  case eval variables arg of
    Right (argVal, env1) ->
      case argVal of
        List (x:_) -> Right (x, env1)
        DottedPair x _ -> Right (x, env1)
        _ -> Left "car: argument is not a non-empty list or pair"
    Left err -> Left err

-- CDR
-- все, кроме первого элемента списка
eval variables (List [Atom "cdr", arg]) =
  case eval variables arg of
    Right (argVal, env1) ->
      case argVal of
        List (_:xs) -> Right (List xs, env1)
        DottedPair _ x -> Right (x, env1)
        _ -> Left "cdr: argument is not a non-empty list or pair"
    Left err -> Left err

-- CONS
-- добавить элемент в начало списка
eval variables (List [Atom "cons", arg1, arg2]) =
  case (eval variables arg1, eval variables arg2) of
    (Right (x, env1), Right (y, env2)) ->
      case y of
        List ys -> Right (List (x:ys), env2)
        _ -> Right (DottedPair x y, env2)
    (Left err, _) -> Left err
    (_, Left err) -> Left err

-- EQ
eval variables (List [Atom "eq", arg1, arg2]) =
  case (eval variables arg1, eval variables arg2) of
    (Right (x, env1), Right (y, env2)) ->
      Right (if x == y then Atom "t" else List [], env2)
    (Left err, _) -> Left err
    (_, Left err) -> Left err

-- + - * / < > =
eval variables (List [Atom "+", arg1, arg2]) = do
  (aVal, env1) <- eval variables arg1
  (bVal, env2) <- eval env1 arg2
  a <- toNumber aVal
  b <- toNumber bVal
  Right (Number (a + b), env2)

eval variables (List [Atom "-", arg1, arg2]) = do
  (aVal, env1) <- eval variables arg1
  (bVal, env2) <- eval env1 arg2
  a <- toNumber aVal
  b <- toNumber bVal
  Right (Number (a - b), env2)

eval variables (List [Atom "*", arg1, arg2]) = do
  (aVal, env1) <- eval variables arg1
  (bVal, env2) <- eval env1 arg2
  a <- toNumber aVal
  b <- toNumber bVal
  Right (Number (a * b), env2)

eval variables (List [Atom "/", arg1, arg2]) = do
  (aVal, env1) <- eval variables arg1
  (bVal, env2) <- eval env1 arg2
  a <- toNumber aVal
  b <- toNumber bVal
  if b == 0 then Left "Division by zero" else Right (Number (div a b), env2)

eval variables (List [Atom "<", arg1, arg2]) = do
  (aVal, env1) <- eval variables arg1
  (bVal, env2) <- eval env1 arg2
  a <- toNumber aVal
  b <- toNumber bVal
  if a < b
    then Right (Atom "t", env2)
    else Right (List [], env2)

eval variables (List [Atom ">", arg1, arg2]) = do
  (aVal, env1) <- eval variables arg1
  (bVal, env2) <- eval env1 arg2
  a <- toNumber aVal
  b <- toNumber bVal
  if a > b
    then Right (Atom "t", env2)
    else Right (List [], env2)

eval variables (List [Atom "=", arg1, arg2]) = do
  (aVal, env1) <- eval variables arg1
  (bVal, env2) <- eval env1 arg2
  a <- toNumber aVal
  b <- toNumber bVal
  if a == b
    then Right (Atom "t", env2)
    else Right (List [], env2)

-- DEFINE
eval variables (List [Atom "define", Atom name, expr]) = do
    (val, newEnv) <- eval variables expr
    case setVar name val newEnv of
        Left err -> Left err
        Right finalEnv -> Right (Atom name, finalEnv)

-- Пустой список
eval variables (List []) = Right (List [], variables)

-- Вызов функции
eval variables (List (funcExpr : args)) = do
  (func, env1) <- eval variables funcExpr
  Left $ "Unknown function: " ++ show func

eval _ expr = Left $ "Invalid expression: " ++ show expr

-- ============================================
-- REPL
-- ============================================

repl :: Variables -> IO ()
repl variables = do
  putStr "lisp> "
  line <- getLine
  case line of
    "exit" -> putStrLn "Bye"
    "" -> repl variables
    _ -> do
      case parseLisp line of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right expr -> do
          case eval variables expr of
            Left err -> putStrLn $ "Eval error: " ++ err
            Right (result, newEnv) -> do
              print result
              repl newEnv

initialEnv :: Variables
initialEnv = [
  ("t", Atom "t"),
  ("nil", List [])
  ]

main :: IO ()
main = do
  putStrLn "=== Mini Lisp REPL ==="
  putStrLn "Type 'exit' to quit"
  putStrLn ""
  repl initialEnv