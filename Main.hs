module Main where

import Data.Char (isSpace)
import Data.Data
import System.IO
import System.Console.CmdArgs (cmdArgs, args, argPos, (&=))
import Text.ParserCombinators.Parsec

type Symbol = String

data Rule = Rule {
  queueHead :: Symbol,
  inputChar :: Maybe Char,
  state :: Symbol,
  push :: [Symbol],
  state' :: Symbol
} deriving Show

type Program = [Rule]

runProgram :: String -> Program -> Bool
runProgram = undefined

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

data RunMode = Deterministic | NonDeterministic
  deriving (Show, Data, Typeable)

data Arguments = Arguments {
  input :: String,
  files :: [String],
  mode :: RunMode
} deriving (Show, Data, Typeable)

main :: IO ()
main = do
  arguments <- cmdArgs $ Arguments {
    input = "" &= argPos 0,
    files = [] &= args,
    mode = Deterministic
  }

  parsed <- sequence [ content >>= parseFile fileName | (fileName, content) <- readFiles $ files arguments ]

  case sequence parsed of
    Nothing ->
      return ()
    Just rss ->
      putStrLn $ if runProgram w p then "Accepted." else "Not accepted"
      where p = concat rss
            w = input arguments

-- vim: expandtab:ts=2:sw=2
