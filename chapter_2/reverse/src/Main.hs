module Main (main) where

import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse xs = reverse' xs []
    where
        reverse' :: [a] -> [a] -> [a]
        reverse' [] rs = rs
        reverse' (x:xs) rs = reverse' xs (x:rs)

main :: IO ()
main = do
    let inp = [1..10]
    let rs = reverse inp
    putStrLn $ "Reverse of " ++ (show inp) ++ " is " ++ (show rs)
