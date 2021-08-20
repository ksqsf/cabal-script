-- vector >= 0.12.3
-- random >= 1.2
import Control.Monad
import System.Random
import qualified Data.Vector.Mutable as V

shuffle :: V.IOVector a -> IO ()
shuffle vec = do
  let len = V.length vec
  forM_ [0..len-1] $ \i -> do
    j <- randomRIO (i, len-1)
    V.swap vec i j

main = do
  vec <- V.generate 10 id
  shuffle vec
  forM_ [0..9] $ \i -> do
    V.read vec i >>= putStrLn . show
