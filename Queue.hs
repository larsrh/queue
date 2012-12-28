{-# LANGUAGE DoAndIfThenElse #-}

module Queue (
  Rule,
  Program,
  program,
  runQueue,
  searchQueue
) where

import Control.Applicative (liftA2)
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import qualified Data.NonEmpty as NE
import Text.ParserCombinators.Parsec hiding (State)

import Common
import Machine

{- Definition -}

type Symbol = String

data Rule = Rule {
  queueHead :: Symbol,
  inputChar :: Maybe Char,
  state :: Symbol,
  push :: [Symbol],
  state' :: Symbol
} deriving (Show, Eq)

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

{- Execution -}

data State = State {
  currentState :: Symbol,
  remainingWord :: String,
  queue :: [Symbol]
} deriving (Show, Eq)

instance Machine Rule State where
  accepts s = null (queue s) && null (remainingWord s)
  match s r =
    case queue s of
      (q : qs) | q == queueHead r && currentState s == state r ->
        fmap mkState $ case (remainingWord s, inputChar r) of
          (w : ws, Just w') | w == w' -> Just ws
          (ws, Nothing) -> Just ws
          _ -> Nothing
        where mkState remaining = State {
                currentState = state' r,
                remainingWord = remaining,
                queue = qs ++ push r
              }
      _ -> Nothing

defaultState :: Rule -> String -> State
defaultState r word = State {
  currentState = state r,
  remainingWord = word,
  queue = [queueHead r]
}

runQueue :: Monad m => Operation m -> String -> NonEmptyList Rule -> IO Bool
runQueue op word rs@(NE.Cons r _) = runSuccess op (NE.flatten rs) $ defaultState r word

searchQueue :: Monad m => Operation m -> String -> String -> NonEmptyList Rule -> IO String
searchQueue op word sep rs = search $ [] : suffixes
  where search [] = undefined
        search (suff : suffs) = do
          let w = word ++ sep ++ suff
          res <- runQueue op w rs
          if res then
            return w
          else
            search suffs
        suffixes = concat $ unfoldr (Just . dupl . gen) [[]]
        dupl x = (x, x)
        gen = liftA2 (:) chars
        chars = catMaybes $ map inputChar $ NE.flatten rs

-- vim: expandtab:ts=2:sw=2
