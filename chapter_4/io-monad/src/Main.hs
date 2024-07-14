module Main (main) where

import System.IO (hGetLine, hIsEOF, withFile, Handle, IOMode(..))
import System.Environment (getArgs)
import Control.Monad
import Data.List (intercalate)

getLinesSeq :: Handle -> IO [String]
getLinesSeq h = do
    eof <- hIsEOF h
    if eof then return [] else (:) <$> hGetLine h <*> getLinesSeq h

printLine :: (Int, String) -> IO ()
printLine (lineno, line) = putStrLn $ intercalate " : " [show lineno, line]

withLineNumbers :: Monad m => m [String] -> m [(Int, String)]
withLineNumbers m = zip <$> pure [1..] <*> m

main :: IO ()
main = do
  args <- getArgs 
  when (length args /= 1) $ do
    putStrLn $ "Incorrect arguments " ++ (show args)
    error "Provide filename"
  withFile (head args) ReadMode (\h -> do
    lines <- withLineNumbers (getLinesSeq h)
    forM_ lines printLine
    )
