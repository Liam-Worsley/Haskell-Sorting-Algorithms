module SortingAlgorithms.SelectionSorter where

--recursively finds the place of the minimum element and places it in the correct order until the array is sorted
selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort lst = let findMin = findMinimum lst (-1) (maxBound) 0
                    in [snd findMin] ++ selectionSort (removeEl lst (fst findMin) 0)

--finds the minimum element in the list and returns the place and value of the element
findMinimum :: [Int] -> Int -> Int -> Int -> (Int,Int)
findMinimum [] place val _ = (place,val)
findMinimum (x:xs) place val curPlace = if x < val then findMinimum xs curPlace x (curPlace + 1) else findMinimum xs place val (curPlace + 1)

--removes the element from the list and returns the new list
removeEl :: [Int] -> Int -> Int -> [Int]
removeEl [] _ _ = []
removeEl (x:xs) place iter = if place == iter then xs else x : removeEl xs place (iter + 1)
