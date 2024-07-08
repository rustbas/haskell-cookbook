module Main (main) where

import Data.Functor

square :: Num a => a -> a
square x = x * x

squareList :: Num a => [a] -> [a]
squareList xs = square <$> xs

squareMaybe :: Num a => Maybe a -> Maybe a
squareMaybe x = square <$> x

squareEither :: Num a => Either c a -> Either c a
squareEither x = square <$> x

data Function a b = Function (a -> b)
instance Functor (Function a) where
    f `fmap` (Function g) = Function (f . g)

double :: Num a => a -> a
double x = x + x

main :: IO ()
main = do
  putStrLn "Mapping a list"
  putStrLn $ show $ squareList [1..10]
  putStrLn ""
  
  putStrLn "Mapping Maybe"
  putStrLn $ show $ squareMaybe (Just 10)
  putStrLn $ show $ squareMaybe (Nothing)
  putStrLn ""

  putStrLn "Mapping Either"
  putStrLn $ show $ squareEither (Right 10 :: Either String Int)
  putStrLn $ show $ squareEither (Left "Left" :: Either String Int)
  putStrLn ""

  let squareF = Function square
      doubleSquare = double <$> squareF

  let Function dsq = doubleSquare
  putStrLn $ "Double the Square of 10 = " ++ (show $ dsq 10)
