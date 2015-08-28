import Data.List (intersperse, elemIndex)
import Data.Maybe (isJust)
import Data.Char (isDigit, isLetter, isSpace)
import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad (MonadPlus, liftM, ap, mplus, mzero, guard)


newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) = f

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance Monad Parser where
  return v = Parser $ \inp -> [(v, inp)]
  p >>= f = Parser $ \inp -> concat [parse (f val) rest
                                    | (val, rest) <- parse p inp]

instance MonadPlus Parser where
  mzero = Parser $ const []
  p `mplus` q = Parser $ \inp -> parse p inp ++ parse q inp


(!++) :: Parser a -> Parser a -> Parser a
p !++ q = Parser $ \inp -> case parse (p <|> q) inp of
  [] -> []
  (x:_) -> [x]


item :: Parser Char
item = Parser $ \inp -> case inp of
  (x:xs) -> [(x, xs)]
  [] -> []

charp :: (Char -> Bool) -> Parser Char
charp cond = do
  c <- item
  guard $ cond c
  return c

char :: Char -> Parser Char
char c = charp (== c)

digit :: Parser Char
digit = charp isDigit

letter :: Parser Char
letter = charp isLetter

many :: Parser a -> Parser [a]
many p = many1 p !++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) !++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
  x <- p
  xs <- many (sep >> p)
  return (x:xs)

space :: Parser String
space = many1 (charp isSpace)

maybeSpaces = many (charp isSpace)

spacesArround :: Parser a -> Parser a
spacesArround p = maybeSpaces *> p <* maybeSpaces

data Sexp = Number Integer
          | Atom String
          | Cons Sexp Sexp
          deriving Show

pprintSexp :: Sexp -> String
pprintSexp s = case toList s of
  (Just ls) -> "(" ++ (concat (intersperse " " $ map pprintSexp ls)) ++ ")"
  _ -> case s of
    (Number i) -> show i
    (Atom a) -> a
    (Cons l r) -> "(" ++ pprintSexp l ++ " . " ++ pprintSexp r ++ ")"

toList :: Sexp -> Maybe [Sexp]
toList (Cons l r) = (l:) <$> toList r
toList (Atom "nil") = Just []
toList _ = Nothing

number :: Parser Integer
number = read <$> many1 digit

atomStart :: Parser Char
atomStart = letter <|> charp (\c -> c `elem` "?!-_:|*+/=")

atom :: Parser String
atom = atomStart >>= \l -> (l:) <$> many (atomStart <|> digit)

quote :: Sexp -> Sexp
quote s = Cons (Atom "quote") (Cons s (Atom "nil"))

sexp :: Parser Sexp
sexp = Number <$> number
       <|> Atom <$> atom
       <|> toConsList <$> (
         (char '(') *>
         spacesArround (sexp `sepby` space)
         <* (char ')')
         )
       <|> quote <$> (char '\'' *> sexp)

toConsList :: [Sexp] -> Sexp
toConsList [] = (Atom "nil")
toConsList (x:xs) = Cons x $ toConsList xs

parseSexp :: String -> Sexp
parseSexp inp = case parse (spacesArround sexp) inp of
  [(s, "")] -> s
  _ -> error "You shall not parse"


data Value = NumberV Integer
           | AtomV String
           | ConsV Value Value
           | Closure String Sexp Env

type Env = String -> Maybe Value

instance Show Value where
  show v = pprintVal v

toListVal :: Value -> Maybe [Value]
toListVal (ConsV l r) = (l:) <$> toListVal r
toListVal (AtomV "nil") = Just []
toListVal _ = Nothing


pprintVal :: Value -> String
pprintVal s = case toListVal s of
  (Just ls) -> "(" ++ (concat (intersperse " " $ map pprintVal ls)) ++ ")"
  _ -> case s of
    (NumberV i) -> show i
    (AtomV a) -> a
    (ConsV l r) -> "(" ++ pprintVal l ++ " . " ++ pprintVal r ++ ")"
    (Closure _ _ _) -> "#closure"



sexpToVal :: Sexp -> Value
sexpToVal (Number n) = NumberV n
sexpToVal (Atom a) = AtomV a
sexpToVal (Cons l r) = ConsV (sexpToVal l) (sexpToVal r)


emptyEnv :: Env
emptyEnv = const Nothing

-- fromAssoc :: [(String, Sexp)] -> Env
-- fromAssoc ls x = lookup x ls

zCombinator :: Sexp
zCombinator = parseSexp
   "(lambda (f) (\
               \ (lambda (x) (f (lambda (n) ((x x) n))))\
               \ (lambda (x) (f (lambda (n) ((x x) n))))))"

buildins :: [(String, (Int, [Value] -> Maybe Value))]
buildins = [
  ("cons", (2, \[l, r] -> Just $ ConsV l r)),
  ("car", (1, \[ls] -> do
              let ConsV l _ = ls
              return l)),
  ("cdr", (1, \[ls] -> do
              let ConsV _ r = ls
              return r)),
  ("sub1", (1, \[arg] -> do
               let NumberV n = arg
               return $ NumberV $ pred n)),
  ("zero?", (1, \[arg] -> do
                let NumberV n = arg
                return $ NumberV  $ if n == 0 then 1 else 0)),
  ("*", (2, \[arg1, arg2] -> do
            let NumberV x = arg1
            let NumberV y = arg2
            return $ NumberV (x * y))),
  ("=", (2, \[arg1, arg2] -> do
            let AtomV x = arg1
            return $ NumberV $ case arg2 of
              AtomV y -> if x == y then 1 else 0
              _ -> 0)),
  ("number?", (1, \[arg] -> Just $ NumberV $
                            case arg of
                              NumberV _ -> 1
                              _ -> 0)),
  ("atom?", (1, \[arg] -> Just $ NumberV $
                          case arg of
                            AtomV _ -> 1
                            _ -> 0))
  ]

fromList :: [Sexp] -> Sexp
fromList [] = Atom "nil"
fromList (x:xs) = Cons x (fromList xs)

call :: String -> Sexp -> Sexp
call f arg = fromList [Atom f, arg]

macros :: [(String, (Int, [Sexp] -> Sexp))]
macros = [
  ("fix", (1, \[arg] -> fromList [zCombinator, arg])),
  ("cadr", (1, \[arg] -> call "car" $ call "cdr" $ arg)),
  ("caddr", (1, \[arg] -> call "car" $ call "cdr" $ call "cdr" $ arg)),
  ("caadr", (1, \[arg] -> call "car" $ call "car" $ call "cdr" $ arg))
  ]

eval :: Env -> Sexp -> Maybe Value
eval _ s@(Number _) = Just $ sexpToVal s
eval _ s@(Atom "nil") = Just $ sexpToVal s
eval env (Atom v) = env v

eval _ (Cons (Atom "quote") args) = do
  [e] <- toList args
  return $ sexpToVal e

eval env (Cons (Atom n) args)
  | isJust $ lookup n buildins = do
    args <- toList args
    (arity, f) <- lookup n buildins
    guard $ arity == length args
    args <- sequence $ map (eval env) args
    f args

eval env (Cons (Atom n) args)
  | isJust $ lookup n macros = do
    (arity, macro) <- (lookup n macros)
    args <- toList args
    guard $ arity == length args
    eval env $ macro args

eval env (Cons (Atom "cond") clauses) =
  toList clauses >>= evalCond env

eval env (Cons (Atom "lambda") args) = do
  [vars, body] <- toList args
  [Atom arg] <- toList vars
  return $ Closure arg body env

eval env (Cons e1 args) = do
   (Closure arg body cenv) <- eval env e1
   [e2] <- toList args
   v <- eval env e2
   eval (\y -> if y == arg then Just v else cenv y) body

evalCond :: Env -> [Sexp] -> Maybe Value
evalCond _ [] = Just (AtomV "nil")
evalCond env (x:xs) = do
  [cond, e] <- toList x
  (NumberV c) <- eval env cond
  if c == 0
    then evalCond env xs
    else eval env e

stripComments :: String -> String
stripComments = unlines . map (takeWhile (/= ';')) . lines


main :: IO ()
main = do
  input <- stripComments <$> readFile "./src/eval-eval-factorial.mu"
  -- putStrLn input
  putStrLn $ case (eval emptyEnv $ parseSexp input) of
    Just value -> pprintVal value
    Nothing -> "Evaluation error"
