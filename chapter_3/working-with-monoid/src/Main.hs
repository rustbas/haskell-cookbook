module Main (main) where

import Data.Monoid ()

data Option = Option { boolOption :: Bool, selections :: [String] } deriving Show

-- https://stackoverflow.com/questions/53622428/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr
instance Semigroup Option where
    (Option b1 s1) <> (Option b2 s2) = Option (b1 || b2) (s1 ++ s2)
instance Monoid Option where
    mempty = Option False []
    -- (Option b1 s1) `mappend` (Option b2 s2) = Option (b1 || b2) (s1 ++ s2)
    mappend = (<>)

main :: IO ()
main = do
  putStrLn "Define empty"
  let defaultOptions = mempty :: Option
  putStrLn $ show defaultOptions

  let option1 = defaultOptions <> (Option True [])
      option2 = option1 <> (Option False ["haskell"])
      option3 = option1 <> (Option True ["cookbook"])

  putStrLn $ show option1
  putStrLn $ show option2
  putStrLn $ show option3
  putStrLn $ show $ mconcat [defaultOptions, option1, option2]
