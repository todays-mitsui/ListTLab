module FromListT (
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

import Data.Maybe (fromJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import ListT


-- | A "lazy" run function, which only calculates the first solution.
runListT' :: ListT m a -> m (Maybe (a, ListT m a))
runListT' = uncons

-- | In ListT from Control.Monad this one is the data constructor ListT
liftList :: Monad m => [a] -> ListT m a
liftList = fromFoldable

--------------------------------------------------------------------------------

-- | Sum of squares

testSumOfSquares :: Int -> ListT IO (Int, Int)
testSumOfSquares n = do
    let squares = liftList . takeWhile (<= n) $ map (^ (2 :: Int)) [0 ..]
    x <- squares
    y <- squares
    lift $ print (x, y)
    guard $ x + y == n
    lift $ putStrLn "Sum of Squares."
    return (x, y)

runTestSumOfSquares :: Int -> IO (Int, Int)
runTestSumOfSquares = fmap fromJust . ListT.head . testSumOfSquares
-- runTestSumOfSquares 5
-- $ (0,0)
--   (0,1)
--   (0,4)
--   (1,0)
--   (1,1)
--   (1,4)
--   Sum of Squares.
-- => (1,4)

--------------------------------------------------------------------------------

-- | Grouping effects

testGroupingEffects1 :: ListT IO Int
testGroupingEffects1 = do
    r <- liftIO $ newIORef 0
    (next r `mplus` next r >> next r `mplus` next r) >> next r `mplus` next r

runTestGroupingEffects1 :: IO [Int]
runTestGroupingEffects1 = toList testGroupingEffects1
-- => [2,3,5,6,9,10,12,13]


testGroupingEffects2 :: ListT IO Int
testGroupingEffects2 = do
    r <- liftIO $ newIORef 0
    next r `mplus` next r >> (next r `mplus` next r >> next r `mplus` next r)

runTestGroupingEffects2 :: IO [Int]
runTestGroupingEffects2 = toList testGroupingEffects2
-- => [2,3,5,6,9,10,12,13]


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

runTestOrderOfPrinting1 :: IO (Maybe ((), ListT IO ()))
runTestOrderOfPrinting1 = runListT' testOrderOfPrinting1
-- $ abc


testOrderOfPrinting2 :: ListT IO ()
testOrderOfPrinting2 = (a `mplus` a) >> (b >> c)

runTestOrderOfPrinting2 :: IO (Maybe ((), ListT IO ()))
runTestOrderOfPrinting2 = runListT' testOrderOfPrinting2
-- $ abc

--------------------------------------------------------------------------------

-- | Order of ListT []

v :: Int -> ListT [] Int
v 0 = ListT [Just (1, ListT [Just (0, ListT [Nothing])])]
v 1 = ListT [Just (0, ListT [Nothing]), Just (1, ListT [Nothing])]


runTestOrderOfListTList1 :: IO ()
runTestOrderOfListTList1 = print $ toList $ ((v >=> v) >=> v) 0
-- $ [[1,1,1,0],[1,0,1,0],[0,1,1,0],[0,0,1,0],[1,0,1,1,0],[1,0,0,1,0]]

runTestOrderOfListTList2 :: IO ()
runTestOrderOfListTList2 = print $ toList $ (v >=> (v >=> v)) 0
-- $ [[1,1,1,0],[1,0,1,0],[0,1,1,0],[0,0,1,0],[1,0,1,1,0],[1,0,0,1,0]]
