module SortingAlgorithms.MergeSorter where

--merges the numbers into singletons then recursively merges them back together until the list is sorted.
mergeSort :: [Int] -> [Int]
mergeSort lst = mergeBack (mergeApart lst)

--splits the array into singletons
mergeApart :: [Int] -> [[Int]]
mergeApart lst = [[x] | x <- lst]

--recursively merges the list back into itself
mergeBack :: [[Int]] -> [Int]
mergeBack [lst] = lst
mergeBack (x:y:xs) = let newlst = xs ++ [mergeTogether x y] in mergeBack newlst

--merging 2 lists into 1
mergeTogether :: [Int] -> [Int] -> [Int]
mergeTogether [] [] = []
mergeTogether lst1 [] = lst1
mergeTogether [] lst2 = lst2
mergeTogether (x:xs) (y:ys) = if x < y then [x] ++ (mergeTogether xs (y:ys)) else [y] ++ (mergeTogether (x:xs) ys)