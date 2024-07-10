module Main (main) where

import Control.Monad

nexts :: Num a => a -> [a]
nexts x = do
    x : nexts (x+1)

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
    x <- xs
    y <- ys
    return (x,y)

partition :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
partition f xs ys = [ (x,y) | x <- xs, y <- ys, f x y]

partition1 :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
partition1 f xs ys = do
    x <- xs
    y <- ys
    if f x y then
        return (x,y)
    else
        []

main :: IO ()
main = do
  print $ take 10 $ nexts 11
  print $ filterM (\x -> if odd x then [True] else [False]) [1..10]
  print $ forM [1..10] (:[])
  print $ forM (Just 10) (:[])
  print $ pairs [1..5] ['a'..'c']
  print $ partition (>) [1..10] [1..10]
  print $ partition1 (<) [1..10] [1..10]

