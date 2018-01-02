module Main where

import FromListT
import FromMtl
import FromTransformers


main :: IO ()
main = do
    putStrLn "# Sum of squares"
    putStrLn ""

    putStrLn "## Control.Monad.Trans.List.ListT from transformers"
    ns <- FromTransformers.runTestSumOfSquares 5
    putStrLn $ "Result: " ++ show ns
    putStrLn ""

    putStrLn "## Control.Monad.List.ListT from mtl"
    ns <- FromMtl.runTestSumOfSquares 5
    putStrLn $ "Result: " ++ show ns
    putStrLn ""

    putStrLn "## ListT.ListT from list-t"
    ns <- FromListT.runTestSumOfSquares 5
    putStrLn $ "Result: " ++ show ns
    putStrLn ""


    putStrLn $ take 80 $ repeat '='

    putStrLn "# Grouping effects"
    putStrLn ""

    putStrLn "## Control.Monad.Trans.List.ListT from transformers"
    putStrLn "### test 1"
    print =<< FromTransformers.runTestGroupingEffects1
    putStrLn ""
    putStrLn "### test 2"
    print =<< FromTransformers.runTestGroupingEffects2
    putStrLn ""

    putStrLn "## Control.Monad.List.ListT from mtl"
    putStrLn "### test 1"
    print =<< FromMtl.runTestGroupingEffects1
    putStrLn ""
    putStrLn "### test 2"
    print =<< FromMtl.runTestGroupingEffects2
    putStrLn ""

    putStrLn "## ListT.ListT from list-t"
    putStrLn "### test 1"
    print =<< FromListT.runTestGroupingEffects1
    putStrLn ""
    putStrLn "### test 2"
    print =<< FromListT.runTestGroupingEffects2
    putStrLn ""


    putStrLn $ take 80 $ repeat '='

    putStrLn "# Order of printing"
    putStrLn ""

    putStrLn "## Control.Monad.Trans.List.ListT from transformers"
    putStrLn "### test 1"
    FromTransformers.runTestOrderOfPrinting1
    putStrLn ""
    putStrLn "### test 2"
    FromTransformers.runTestOrderOfPrinting2
    putStrLn ""

    putStrLn "## Control.Monad.List.ListT from mtl"
    putStrLn "### test 1"
    FromMtl.runTestOrderOfPrinting1
    putStrLn ""
    putStrLn "### test 2"
    FromMtl.runTestOrderOfPrinting2
    putStrLn ""

    putStrLn "## ListT.ListT from list-t"
    putStrLn "### test 1"
    FromListT.runTestOrderOfPrinting1
    putStrLn ""
    putStrLn "### test 2"
    FromListT.runTestOrderOfPrinting2
    putStrLn ""


    putStrLn $ take 80 $ repeat '='

    putStrLn "# Order of ListT []"
    putStrLn ""

    putStrLn "## Control.Monad.Trans.List.ListT from transformers"
    putStrLn "### test 1"
    FromTransformers.runTestOrderOfListTList1
    putStrLn ""
    putStrLn "### test 2"
    FromTransformers.runTestOrderOfListTList2
    putStrLn ""

    putStrLn "## Control.Monad.List.ListT from mtl"
    putStrLn "### test 1"
    FromMtl.runTestOrderOfListTList1
    putStrLn ""
    putStrLn "### test 2"
    FromMtl.runTestOrderOfListTList2
    putStrLn ""

    putStrLn "## ListT.ListT from list-t"
    putStrLn "### test 1"
    FromListT.runTestOrderOfListTList1
    putStrLn ""
    putStrLn "### test 2"
    FromListT.runTestOrderOfListTList2
    putStrLn ""

    putStrLn "Done."
