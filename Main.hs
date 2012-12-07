{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data
import qualified Data.NonEmpty as NE
import System.IO
import System.Console.CmdArgs (cmdArgs, args, argPos, (&=))

import Common
import Machine
import Queue

data RunMode = Deterministic | Random | All
  deriving (Data, Typeable)

data OperationBox = forall m. Monad m => Box { unbox :: Operation m }

runWithMode :: RunMode -> String -> NonEmptyList Rule -> IO Bool
runWithMode mode word rs = case toBox mode of Box op -> runProgram op word rs
  where toBox Deterministic = Box runFirst
        toBox Random = Box runRandom
        toBox All = Box runAll

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

  parsed <- sequence [ content >>= parseFile fileName program | (fileName, content) <- readFiles $ files arguments ]

  case fmap concat $ sequence parsed of
    Nothing ->
      return ()
    Just [] ->
      hPutStrLn stderr "Empty program."
    Just (r:rs) -> do
      res <- runWithMode (mode arguments) (input arguments) (NE.Cons r rs)
      putStrLn $ if res then "Accepted." else "Not accepted."

-- vim: expandtab:ts=2:sw=2
