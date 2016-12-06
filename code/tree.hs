
type Gift = String

data ChristmasTree a = Nil
  | Leaf a
  | Branch (ChristmasTree a) (ChristmasTree a)
  deriving (Eq, Ord, Read)


-- TODO: write down something to find all the gifts in a given tree

find_gifts :: ChristmasTree Gift -> [Gift]
find_gifts Nil = []
find_gifts (Leaf gift) = [gift]
find_gifts (Branch left right) = (find_gifts left) ++ (find_gifts right)

instance (Show a) => Show (ChristmasTree a) where
  show Nil = "O"
  show (Leaf gift) = "G"
  show (Branch left right) = "*\n" ++ (show left) ++ " ~~ " ++ (show right)
  
main :: IO ()
main = do
  print $ Branch (Leaf "Toy") (Branch (Leaf "Ball") (Leaf "Toy"))
