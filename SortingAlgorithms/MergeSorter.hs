module SortingAlgorithms.MergeSorter where


mergeSort :: [Int] -> [Int]
mergeSort lst = mergeBack (mergeApart lst)

mergeApart :: [Int] -> [[Int]]
mergeApart lst = [lst]

mergeBack :: [[Int]] -> [Int]
mergeBack lst = head lst