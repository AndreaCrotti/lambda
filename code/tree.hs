module Tree where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show, Eq)

traverse :: (Tree a) -> (a -> b) -> (Tree a)
traverse (Leaf x) func = Leaf x
