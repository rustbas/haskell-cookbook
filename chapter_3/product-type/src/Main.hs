module Main (main) where

data Product1 = Product1 Bool deriving Show
data Product2 = Product2 Bool Bool deriving Show
data Product3 a = Product3 a Bool deriving Show
data Product4 a b = Product4 a b deriving Show

main :: IO ()
main = do
  putStrLn "Product1: Simple product type"
  putStrLn $ show $ Product1 True
  putStrLn $ show $ Product1 False

  putStrLn "Product2: Product type with two boolean fields"
  putStrLn $ show $ Product2 True True
  putStrLn $ show $ Product2 True False
  putStrLn $ show $ Product2 False True
  putStrLn $ show $ Product2 False False

  putStrLn "Product3: Simple parametrizied product type"
  let product3 = Product3 10 True :: Product3 Int 
  putStrLn $ show $ product3

  putStrLn "Product4: Product type with boolean and Int fields"
  putStrLn $ show $ (Product4 10 True :: Product4 Int Bool)
