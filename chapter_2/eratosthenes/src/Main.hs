module Main (main) where

primes :: [Integer]
primes = 2 : filterMultiples allMultiples [3,5..]
    where
    allMultiples = mergeMultiples $ map multiples primes
    multiples i = map (i*) [i..]

filterMultiples :: Ord a => [a] -> [a] -> [a]
filterMultiples (f:fs) (x:xs)   | f < x = filterMultiples fs (x:xs)
                                | f > x = x : filterMultiples (f:fs) xs
                                | otherwise = filterMultiples fs xs

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)  | x < y = x : merge xs (y:ys)
                        | x > y = y : merge (x:xs) ys
                        | otherwise = x : merge xs ys

mergeMultiples :: Ord a => [[a]] -> [a]
mergeMultiples ((x:xs):xss) = x : merge xs (mergeMultiples xss)

main :: IO ()
main = do
  let primes1k = take 1000 primes
  putStrLn $ "1000th prime number = " ++ (show $ ( take 100000 primes !! 10) )
