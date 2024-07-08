module Main (main) where

import Data.Functor
import Control.Applicative

multiplyLists :: Num a => [a] -> [a] -> [a]
multiplyLists xs ys = (*) <$> xs <*> ys

tupleMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
tupleMaybe x y = (,) <$> x <*> y

addEither :: Num a => Either c a -> Either c a -> Either c a
addEither x y = pure (+) <*> x <*> y

main :: IO ()
main = do
  putStrLn $ show $ multiplyLists [1..3] [11..13]

  putStrLn ""

  putStrLn $ show $ tupleMaybe (pure 10) (Just "String")
  putStrLn $ (show :: Maybe (Int, String) -> String) $ tupleMaybe (pure 10) Nothing

  putStrLn ""

  putStrLn $ (show :: Either String Int -> String) $ addEither (Right 100) (Right 10)
  putStrLn $ (show :: Either String Int -> String) $ addEither (Left "String") (Right 10)
