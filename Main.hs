module Main where

import Data.Char (isSpace)
import Data.Traversable (traverse)
import System.IO
import System.Environment (getArgs)
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

readFiles :: [String] -> IO String
readFiles [] = getContents
readFiles files = fmap concat $ traverse readFile files

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hPutStrLn stderr "No input string given. If you want the empty string, specify it explicitly, e.g. via ''"
    x : xs -> do
      contents <- readFiles xs
      case parse program "" contents of
        Left err ->
          hPutStrLn stderr $ "Parse error at " ++ (show err)
        Right p -> do
          let rep = unlines $ map show p
          hPutStrLn stderr rep
          putStrLn output
          where output = if runProgram x p then "Accepted." else "Not accepted"

-- vim: expandtab:ts=2:sw=2
