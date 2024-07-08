module Main (main) where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Applicative Tree where
    pure x = Node (pure x) x (pure x)

    (<*>) Leaf _ = Leaf
    (<*>) _ Leaf = Leaf 
    (<*>) (Node lf f rf) (Node la a ra) = Node (lf <*> la) (f a) (rf <*> ra)

node :: Tree a -> a -> Tree a -> Tree a
node l x r = Node  l x r

singleton :: a -> Tree a
singleton x = Node Leaf x Leaf

sampleTree :: Int -> Tree Int
sampleTree b = node l b r
    where
    l =   node ll (b+1)  rl
    r =   node lr (b+2)  rr
    ll =  node lll (b+3)  rll
    rl =  node lrl (b+4)  rlr
    lr =  node llr (b+5)  rlr
    rr =  node lrr (b+6)  rrr
    lll = singleton (b+7) 
    rll = singleton (b+8) 
    lrl = singleton (b+9)
    rrl = singleton (b+10)
    llr = singleton (b+11)
    rlr = singleton (b+12)
    lrr = singleton (b+13)
    rrr = singleton (b+14)

main :: IO ()
main = do
  let intTree1 = sampleTree 1
      intTree2 = sampleTree 15
      finalTree = (+) <$> intTree1 <*> intTree2

  putStrLn "intTree1"
  print intTree1
  putStrLn "intTree2"
  print intTree2
  putStrLn "finalTree"
  print finalTree

  putStrLn ""

  putStrLn $ show $ (pure id <*> intTree1) == intTree1
  putStrLn $ show $ (pure id <*> intTree1) == intTree1

  let square x = x*x
      double x = x + x
  print $ (pure (.) <*> pure square <*> pure double <*> intTree1) == (pure square <*> (pure double <*> intTree1))
