module Main (main) where

import Data.Maybe --(isJust, isNothing)

safeOperation :: Num a => (a -> a -> Bool) -> (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
safeOperation _ _ Nothing _ = Nothing
safeOperation _ _ _ Nothing = Nothing
safeOperation c _ (Just i) (Just j) | c i j = Nothing
safeOperation c op (Just i) (Just j) = Just (i `op` j)

safeDiv :: Maybe Int -> Maybe Int -> Maybe Int
safeDiv = safeOperation divCondition div 
    where
    divCondition _ 0 = True
    divCondition _ _ = False

-- do-notation
safeDiv1 :: Maybe Int -> Maybe Int -> Maybe Int
safeDiv1 i j = do
    xi <- i
    xj <- j
    if 0 == xj
        then 
        Nothing
    else
        return (xi `div` xj)


main :: IO ()
main = do
  putStrLn "Using Maybe"
  let i = Just 10 :: Maybe Int
      j = Just 2 :: Maybe Int
      z = Just 0 :: Maybe Int

  putStrLn $ "Does (Just 10) represent a value? " ++ (show $ isJust i)
  putStrLn $ "Does (Nothing) represent a value? " ++ (show $ isJust Nothing)
  putStrLn $ "Does (Just 10) is really Nothing? " ++ (show $ isNothing Nothing)

  putStrLn ""

  putStrLn $ "singleton List and Maybe interoperability"
  putStrLn $ "Converting list [10] to Maybe: " ++ (show $ listToMaybe [10])
  putStrLn $ "Converting empty list to Maybe: " ++ (show $ (listToMaybe [] :: Maybe Int))
  putStrLn $ "Converting Maybe (Just 10) to list: " ++ (show $ maybeToList $ Just 10)
  putStrLn $ "Converting Maybe (Nothing) to list: " ++ (show $ maybeToList $ (Nothing :: Maybe Int))

  putStrLn ""

  putStrLn "Transformation"
  let defaultNull = "NULL"
      convertToString = maybe defaultNull show
      null = convertToString Nothing
      something = convertToString $ Just 10
  putStrLn $ "Convering Nothing to a String: " ++ null
  putStrLn $ "Convering a value to a String: " ++ something

  putStrLn ""

  putStrLn $ "Value from (Just 10) = " ++ (show $ fromJust i)
  putStrLn $ "SaveDiv 10 / 2 = " ++ (show $ safeDiv i j)
  putStrLn $ "SaveDiv 10 / 0 = " ++ (show $ safeDiv i z)

  putStrLn ""

  putStrLn "Do-notation"
  putStrLn $ "SaveDiv 10 / 2 = " ++ (show $ safeDiv1 i j)
  putStrLn $ "SaveDiv 10 / 0 = " ++ (show $ safeDiv1 i z)

  putStrLn ""

  let evens = mapMaybe (\x -> if odd x then Nothing else (Just x)) [1..10]
  putStrLn $ "Filtering odd elements: " ++ (show $ evens)

