module SortingAlgorithms.StalinSorter where

stalinSort :: [Int] -> [Int]
stalinSort [] = []
stalinSort [x] = [x]
stalinSort (x:y:xs) = if x <= y then x : stalinSort (y:xs) else stalinSort (x:xs)