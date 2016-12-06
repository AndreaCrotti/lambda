import System.Environment
import Control.Monad
import Data.Set


main:: IO ()
main = do
  [s] <- getArgs
  f <- readFile "/usr/share/dict/british-english"
  g <- readFile s
  let dict = fromList (lines f)
  mapM_ (spell dict) (words g)


spell:: Set String -> String -> IO ()
spell d w = when (w `notMember` d) (putStrLn w)
