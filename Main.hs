module Main where

import Data.Char (isSpace)
import Data.Data
import System.IO
import System.Console.CmdArgs (cmdArgs, args, argPos, (&=))
import Text.ParserCombinators.Parsec hiding (State)

import Machine

{- Common data -}

type Symbol = String

data Rule = Rule {
  queueHead :: Symbol,
  inputChar :: Maybe Char,
  state :: Symbol,
  push :: [Symbol],
  state' :: Symbol
} deriving Show

type Program = [Rule]

{- Parser -}

program :: GenParser Char st Program
program = spaces >> sepEndBy rule spaces

delimiter :: GenParser Char st ()
delimiter = spaces >> char ',' >> spaces

multiChar :: GenParser Char st Symbol
multiChar = quoted <|> unquoted
  where unquoted = many1 $ satisfy (\c -> not (isSpace c) && c /= '"' && c /= ']' && c /= ',')
        quoted = do char '"'
                    sym <- many1 $ satisfy (/= '"')
                    char '"'
                    return sym

singleChar :: GenParser Char st Char
singleChar = quoted <|> unquoted
  where unquoted = satisfy (\c -> not (isSpace c) && c /= '\'' && c /= ',')
        quoted = do char '\''
                    c <- satisfy (/= '\'')
                    char '\''
                    return c

rule :: GenParser Char st Rule
rule = do
  qh <- multiChar
  delimiter
  ic <- optionMaybe singleChar
  delimiter
  s <- multiChar
  spaces
  string "->"
  spaces
  char '['
  spaces
  p <- sepBy multiChar delimiter
  char ']'
  delimiter
  s' <- multiChar
  return $ Rule { queueHead = qh, inputChar = ic, state = s, push = p, state' = s' }

readFiles :: [String] -> [(String, IO String)]
readFiles [] = [("<stdin>", getContents)]
readFiles fs = [(f, readFile f) | f <- fs ]

parseFile :: String -> String -> IO (Maybe Program)
parseFile fileName content = case parse program fileName content of
  Left err -> do
    hPutStrLn stderr $ "Parse error at " ++ (show err)
    return Nothing
  Right p -> do
    return $ Just p

{- Execution -}

data State = State {
  currentState :: Symbol,
  remainingWord :: String,
  queue :: [Symbol]
} deriving Eq

instance Machine Rule State where
  accepts s = null (queue s) && null (remainingWord s)
  match s r =
    case queue s of
      (q : qs) | q == queueHead r ->
        fmap mkState $ case (remainingWord s, inputChar r) of
          (w : ws, Just w') | w == w' -> Just ws
          (ws, Nothing) -> Just ws
          _ -> Nothing
        where mkState remaining = State {
                currentState = state' r,
                remainingWord = remaining,
                queue = push r ++ qs
              }
      _ -> Nothing

runProgram :: String -> Program -> IO Bool
runProgram = undefined

{- Command line -}

data RunMode = Deterministic | Random | All
  deriving (Data, Typeable)

data OperationBox = forall m. Monad m => Box { unbox :: Operation m }

data Arguments = Arguments {
  input :: String,
  files :: [String],
  mode :: RunMode
} deriving (Data, Typeable)

main :: IO ()
main = do
  arguments <- cmdArgs $ Arguments {
    input = "" &= argPos 0,
    files = [] &= args,
    mode = Deterministic
  }

  parsed <- sequence [ content >>= parseFile fileName | (fileName, content) <- readFiles $ files arguments ]

  case fmap concat $ sequence parsed of
    Nothing ->
      return ()
    Just rs -> do
      res <- runProgram (input arguments) rs
      putStrLn $ if res then "Accepted." else "Not accepted"

-- vim: expandtab:ts=2:sw=2
