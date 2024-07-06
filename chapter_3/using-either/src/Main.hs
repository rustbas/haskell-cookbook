module Main (main) where

import Data.Either

saveDiv :: Either String Int -> Either String Int -> Either String Int
saveDiv (Left e) _ = Left e 
saveDiv _ (Left e) = Left e 
saveDiv (Right i) (Right j) | j == 0 = Left "Illegal Operation: Division by Zero"
                            | otherwise = Right (i `div` j)

main :: IO ()
main = do
  let i = Right 10 :: Either String Int
      j = Right 2  :: Either String Int
      z = Right 0  :: Either String Int

  putStrLn $ "SafeDiv: 10 / 2 = " ++ (show $ saveDiv i j)
  putStrLn $ "SafeDiv: 10 / 0 = " ++ (show $ saveDiv i z)
