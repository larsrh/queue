{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data
import qualified Data.NonEmpty as NE
import System.IO
import System.Console.CmdArgs (cmdArgs, args, argPos, (&=))

import Common
import Machine
import Queue

data StepMode = Deterministic | Random | All
  deriving (Data, Typeable)

data OperationBox = forall m. Monad m => Box { unbox :: Operation m }

toBox :: StepMode -> OperationBox
toBox Deterministic = Box runFirst
toBox Random = Box runRandom
toBox All = Box runAll

run :: StepMode -> String -> NonEmptyList Rule -> IO Bool
run mode word rs = case toBox mode of Box op -> runQueue op word rs

search :: StepMode -> String -> String -> NonEmptyList Rule -> IO String
search mode word sep rs = case toBox mode of Box op -> searchQueue op word sep rs

data Arguments = Arguments {
  input :: String,
  files :: [String],
  stepMode :: StepMode,
  separator :: Maybe String
} deriving (Data, Typeable)

main :: IO ()
main = do
  arguments <- cmdArgs $ Arguments {
    input = "" &= argPos 0,
    files = [] &= args,
    stepMode = Deterministic,
    separator = Nothing
  }

  parsed <- sequence [ content >>= parseFile fileName program | (fileName, content) <- readFiles $ files arguments ]

  case fmap concat $ sequence parsed of
    Nothing ->
      return ()
    Just [] ->
      hPutStrLn stderr "Empty program."
    Just (r:rs) ->
      case separator arguments of
        Nothing -> do
          res <- run (stepMode arguments) (input arguments) (NE.Cons r rs)
          putStrLn $ if res then "Accepted." else "Not accepted."
        Just sep -> do
          res <- search (stepMode arguments) (input arguments) sep (NE.Cons r rs)
          putStrLn $ "Suffix found. Accepted word is " ++ res ++ "."

-- vim: expandtab:ts=2:sw=2
