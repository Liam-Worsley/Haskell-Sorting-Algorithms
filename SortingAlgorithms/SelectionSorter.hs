module SortingAlgorithms.SelectionSorter where

selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort lst = let findMin = findMinimum lst (-1) (maxBound) 0
                    in [snd findMin] ++ selectionSort (removeEl lst (fst findMin) 0)

findMinimum :: [Int] -> Int -> Int -> Int -> (Int,Int)
findMinimum [] place val _ = (place,val)
findMinimum (x:xs) place val curPlace = if x < val then findMinimum xs curPlace x (curPlace + 1) else findMinimum xs place val (curPlace + 1)

removeEl :: [Int] -> Int -> Int -> [Int]
removeEl [] _ _ = []
removeEl (x:xs) place iter = if place == iter then xs else x : removeEl xs place (iter + 1)
--My TEST 4 6 1 8 9 8 10 5 4 9 10 3 2
--WHAT IT RETURNS: 3 4 6 1 8 9 8 10 5 4 9 10 2

-- array = [4,6,1,8,9,8,10,5,4,9,10,3,2] :: [Int]
-- array2 = selectionSort array
-- showArray array2