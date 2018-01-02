# ListTLab

[transformers](http://hackage.haskell.org/package/transformers-0.5.5.0/docs/Control-Monad-Trans-List.html), [mtl](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-List.html), [list-t](https://hackage.haskell.org/package/list-t-1.0.0.1/docs/ListT.html) の 3 パッケージから提供されている `ListT` を比べてみます。

## TL;DR

transformers と mtl の `ListT` の定義は同一で、モナド `m` に対して `ListT m` がモナドにならない場合があります。  
モナド則を満たす `ListT m` が欲しいときは list-t パッケージを使いましょう。

## 1. Sum of squares

transformers や mtl の `runListT` は過剰に正格です。  
一方 list-t は `ListT m a` の必要な要素だけを正しく遅延評価する `uncons`, `head` などのメソッドを持っています。

### テストプログラム

```haskell
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
```

### 実行結果 : Control.Monad.Trans.List.ListT from transformers

```console
ghci> FromTransformers.runTestSumOfSquares 5
(0,0)
(0,1)
(0,4)
(1,0)
(1,1)
(1,4)
Sum of Squares.
(4,0)
(4,1)
Sum of Squares.
(4,4)

(1,4)
```

### 実行結果 : Control.Monad.List.ListT from mtl

```console
ghci> FromMtl.runTestSumOfSquares 5
(0,0)
(0,1)
(0,4)
(1,0)
(1,1)
(1,4)
Sum of Squares.
(4,0)
(4,1)
Sum of Squares.
(4,4)

(1,4)
```

## 実行結果 : ListT.ListT from list-t

```console
ghci> FromListT.runTestSumOfSquares 5
(0,0)
(0,1)
(0,4)
(1,0)
(1,1)
(1,4)
Sum of Squares.

(1,4)
```

---

## 2. Grouping effects

transformers や mtl の `ListT` は、あらゆるモナド `m` について `List m` がモナドの結合則を満たすように定義されてはいません。

### テストプログラム

```haskell
testGroupingEffects1 :: ListT IO Int
testGroupingEffects1 = do
    r <- liftIO $ newIORef 0
    (next r `mplus` next r >> next r `mplus` next r) >> next r `mplus` next r

runTestGroupingEffects1 :: IO [Int]
runTestGroupingEffects1 = toList testGroupingEffects1


testGroupingEffects2 :: ListT IO Int
testGroupingEffects2 = do
    r <- liftIO $ newIORef 0
    next r `mplus` next r >> (next r `mplus` next r >> next r `mplus` next r)

runTestGroupingEffects2 :: IO [Int]
runTestGroupingEffects2 = toList testGroupingEffects2


next :: IORef Int -> ListT IO Int
next r = liftIO $ do
    x <- readIORef r
    writeIORef r (x + 1)
    return x
```

### 実行結果 : Control.Monad.Trans.List.ListT from transformers

```console
ghci> -- test 1
ghci> FromTransformers.runTestGroupingEffects1
[6,7,8,9,10,11,12,13]

ghci> -- test 2
ghci> FromTransformers.runTestGroupingEffects2
[4,5,6,7,10,11,12,13]
```

### 実行結果 : Control.Monad.List.ListT from mtl

```console
ghci> -- test 1
ghci> FromMtl.runTestGroupingEffects1
[6,7,8,9,10,11,12,13]

ghci> -- test 2
ghci> FromMtl.runTestGroupingEffects2
[4,5,6,7,10,11,12,13]
```

### 実行結果 : ListT.ListT from list-t

```console
ghci> -- test 1
ghci> FromListT.runTestGroupingEffects1
[2,3,5,6,9,10,12,13]

ghci> -- test 2
ghci> FromListT.runTestGroupingEffects2
[2,3,5,6,9,10,12,13]
```

---

## 3. Order of printing

transformers や mtl の `ListT` は、 `mplus` (通常のリストにおける `(++)` に相当) に対しても期待通りの動きをしていません。  
一方 list-t の `ListT` は期待通りに動作し、しかも最初の要素の IO アクションだけ実行することもできています。

### テストプログラム

```haskell
a, b, c :: ListT IO ()
[a, b, c] = map (liftIO . putChar) ['a', 'b', 'c']


testOrderOfPrinting1 :: ListT IO ()
testOrderOfPrinting1 = ((a `mplus` a) >> b) >> c

runTestOrderOfPrinting1 :: IO (Maybe ((), ListT IO ()))
runTestOrderOfPrinting1 = runListT' testOrderOfPrinting1


testOrderOfPrinting2 :: ListT IO ()
testOrderOfPrinting2 = (a `mplus` a) >> (b >> c)

runTestOrderOfPrinting2 :: IO (Maybe ((), ListT IO ()))
runTestOrderOfPrinting2 = runListT' testOrderOfPrinting2
```

### 実行結果 : Control.Monad.Trans.List.ListT from transformers

```console
ghci> -- test 1
ghci> FromTransformers.runTestOrderOfPrinting1
aabbcc

ghci> -- test 2
ghci> FromTransformers.runTestOrderOfPrinting2
aabcbc
```

### 実行結果 : Control.Monad.List.ListT from mtl

```console
ghci> -- test 1
ghci> FromMtl.runTestOrderOfPrinting1
aabbcc

ghci> -- test 2
ghci> FromMtl.runTestOrderOfPrinting1
aabcbc
```

### 実行結果 : ListT.ListT from list-t

```console
ghci> -- test 1
ghci> FromListT.runTestOrderOfPrinting1
abc

ghci> -- test 2
ghci> FromListT.runTestOrderOfPrinting1
abc
```

---

## 4. Order of ListT []

transformers や mtl の `ListT` におけるモナドの結合則の破れは `IO` モナドを含まないシンプルな例でも見られます。

### テストプログラム

```haskell
v :: Int -> ListT [] Int
v 0 = ListT [0, 1]
v 1 = ListT [[0], [1]]


runTestOrderOfListTList1 :: IO ()
runTestOrderOfListTList1 = print $ toList $ ((v >=> v) >=> v) 0

runTestOrderOfListTList2 :: IO ()
runTestOrderOfListTList2 = print $ toList $ (v >=> (v >=> v)) 0
```

### 実行結果 : Control.Monad.Trans.List.ListT from transformers

```console
ghci> -- test 1
ghci> FromTransformers.runTestOrderOfListTList1
[[0,1,0,0,1],[0,1,1,0,1],[0,1,0,0],[0,1,0,1],[0,1,1,0],[0,1,1,1]]

ghci> -- test 2
ghci> FromTransformers.runTestOrderOfListTList2
[[0,1,0,0,1],[0,1,0,0],[0,1,0,1],[0,1,1,0,1],[0,1,1,0],[0,1,1,1]]
```

### 実行結果 : Control.Monad.List.ListT from mtl

```console
ghci> -- test 1
ghci> FromMtl.runTestOrderOfListTList1
[[0,1,0,0,1],[0,1,1,0,1],[0,1,0,0],[0,1,0,1],[0,1,1,0],[0,1,1,1]]

ghci> -- test 2
ghci> FromMtl.runTestOrderOfListTList2
[[0,1,0,0,1],[0,1,0,0],[0,1,0,1],[0,1,1,0,1],[0,1,1,0],[0,1,1,1]]
```

### 実行結果 : ListT.ListT from list-t

```console
ghci> -- test 1
ghci> FromListT.runTestOrderOfListTList1
[[1,0,0,1,0],[1,0,1,1,0],[0,0,1,0],[0,1,1,0],[1,0,1,0],[1,1,1,0]]

ghci> -- test 2
ghci> FromListT.runTestOrderOfListTList2
[[1,0,0,1,0],[1,0,1,1,0],[0,0,1,0],[0,1,1,0],[1,0,1,0],[1,1,1,0]]
```