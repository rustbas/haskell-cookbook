module Main (main) where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort yx ++ [x] ++ qsort zx
    where
    yx = filter (\y -> y < x) xs
    zx = filter (\z -> z >=  x) xs

main :: IO ()
main = do
    let input = [5,2,3,1,7,9,8,4,6,0]
        sorted = qsort input
    putStrLn $ "input: " ++ (show input)
    putStrLn $ "output: " ++ (show sorted)
