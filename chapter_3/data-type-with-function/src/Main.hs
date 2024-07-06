module Main (main) where

newtype Func a b = Func (a -> b)
compose :: Func a b -> Func b c -> Func a c
compose (Func f) (Func g) = Func (g . f)

apply :: Func a b -> a -> b
apply (Func f) a = f a

newtype Fix f = Fix (f (Fix f))

data Ghost a = Ghost deriving Show

main :: IO ()
main = do
  let   square x = x * x
        sqrti = floor . sqrt . fromIntegral

  let squareF = Func square 
      sqrtF = Func sqrti

  let idF = compose squareF sqrtF

  putStrLn "Composing"
  print $ apply idF 3

  let x = Ghost
      y= Fix x
      Fix z = y

  putStrLn "Original:"
  print x
  putStrLn "Fixed:"
  print z
