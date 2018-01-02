module FromTransformers (
      runTestSumOfSquares
    , runTestGroupingEffects1
    , runTestGroupingEffects2
    , runTestOrderOfPrinting1
    , runTestOrderOfPrinting2
    , runTestOrderOfListTList1
    , runTestOrderOfListTList2
    ) where

    
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad             ((>=>), guard, mplus)

import Data.List  (uncons)
import Data.Maybe (fromJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Control.Monad.Trans.List


--------------------------------------------------------------------------------

-- | Sum of squares

testSumOfSquares :: Int -> ListT IO (Int, Int)
testSumOfSquares n = do
    let squares = ListT . return . takeWhile (<= n) $ map (^ (2 :: Int)) [0 ..]
    x <- squares
    y <- squares
    lift $ print (x, y)
    guard $ x + y == n
    lift $ putStrLn "Sum of Squares."
    return (x, y)

runTestSumOfSquares :: Int -> IO (Int, Int)
runTestSumOfSquares = fmap Prelude.head . runListT . testSumOfSquares
-- runTestSumOfSquares 5
-- $ (0,0)
--   (0,1)
--   (0,4)
--   (1,0)
--   (1,1)
--   (1,4)
--   Sum of Squares.
--   (4,0)
--   (4,1)
--   Sum of Squares.
--   (4,4)
-- => (1,4)

--------------------------------------------------------------------------------

-- | Grouping effects

testGroupingEffects1 :: ListT IO Int
testGroupingEffects1 = do
    r <- liftIO $ newIORef 0
    (next r `mplus` next r >> next r `mplus` next r) >> next r `mplus` next r

runTestGroupingEffects1 :: IO [Int]
runTestGroupingEffects1 = runListT testGroupingEffects1
-- => [6,7,8,9,10,11,12,13]


testGroupingEffects2 :: ListT IO Int
testGroupingEffects2 = do
    r <- liftIO $ newIORef 0
    next r `mplus` next r >> (next r `mplus` next r >> next r `mplus` next r)

runTestGroupingEffects2 :: IO [Int]
runTestGroupingEffects2 = runListT testGroupingEffects2
-- => [4,5,6,7,10,11,12,13]


next :: IORef Int -> ListT IO Int
next r = liftIO $ do
    x <- readIORef r
    writeIORef r (x + 1)
    return x

--------------------------------------------------------------------------------

-- | Order of printing

a, b, c :: ListT IO ()
[a, b, c] = map (liftIO . putChar) ['a', 'b', 'c']


testOrderOfPrinting1 :: ListT IO ()
testOrderOfPrinting1 = ((a `mplus` a) >> b) >> c

runTestOrderOfPrinting1 :: IO ()
runTestOrderOfPrinting1 = fmap Prelude.head . runListT $ testOrderOfPrinting1
-- $ aabbcc


testOrderOfPrinting2 :: ListT IO ()
testOrderOfPrinting2 = (a `mplus` a) >> (b >> c)

runTestOrderOfPrinting2 :: IO ()
runTestOrderOfPrinting2 = fmap Prelude.head . runListT $ testOrderOfPrinting2
-- $ aabcbc

--------------------------------------------------------------------------------

-- | Order of ListT []

v :: Int -> ListT [] Int
v 0 = ListT [[0, 1]]
v 1 = ListT [[0], [1]]


runTestOrderOfListTList1 :: IO ()
runTestOrderOfListTList1 = print $ runListT $ ((v >=> v) >=> v) 0
-- $ [[0,1,0,0,1],[0,1,1,0,1],[0,1,0,0],[0,1,0,1],[0,1,1,0],[0,1,1,1]]

runTestOrderOfListTList2 :: IO ()
runTestOrderOfListTList2 = print $ runListT $ (v >=> (v >=> v)) 0
-- $ [[0,1,0,0,1],[0,1,0,0],[0,1,0,1],[0,1,1,0,1],[0,1,1,0],[0,1,1,1]]
