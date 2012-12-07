module Common (
  NonEmptyList,
  nubNE,
  readFiles,
  parseFile
) where

import Data.List (nub)
import qualified Data.NonEmpty as NE
import System.IO
import Text.ParserCombinators.Parsec

type NonEmptyList a = NE.T [] a

nubNE :: Eq a => NonEmptyList a -> NonEmptyList a
nubNE (NE.Cons x xs) = aux $ nub xs
  where aux [] = NE.Cons x []
        aux (y : ys) | x == y || x `elem` ys = NE.Cons y ys
        aux ys = NE.Cons x ys

readFiles :: [String] -> [(String, IO String)]
readFiles [] = [("<stdin>", getContents)]
readFiles fs = [(f, readFile f) | f <- fs ]

parseFile :: String -> GenParser Char () a -> String -> IO (Maybe a)
parseFile fileName p content = case parse p fileName content of
  Left err -> do
    hPutStrLn stderr $ "Parse error at " ++ (show err)
    return Nothing
  Right res -> do
    return $ Just res

-- vim: expandtab:ts=2:sw=2
