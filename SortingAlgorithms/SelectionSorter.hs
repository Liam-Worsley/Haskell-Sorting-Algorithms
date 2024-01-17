module SortingAlgorithms.SelectionSorter where

selectionSort :: [Int] -> [Int]
selectionSort lst = runThrough lst 0

runThrough :: [Int] -> Int -> [Int]
runThrough lst n = if n == (length lst) - 1 then lst else let minPlace = findMinimum lst n (minBound) 0 0
                                                              newlist = swap lst n minPlace (lst !! n) (lst !! minPlace) 0
                                                          in runThrough newlist (n + 1)


--finds the place of the minimum element in the rest of the array
findMinimum :: [Int] -> Int -> Int -> Int -> Int -> Int
findMinimum [] _ _ minPlace _ = minPlace
findMinimum (x:xs) startFrom minValue minPlace currentPlace = if x < minValue && startFrom <= currentPlace 
                                                              then findMinimum xs startFrom x currentPlace (currentPlace + 1)
                                                              else findMinimum xs startFrom minValue minPlace (currentPlace + 1)

swap :: [Int] -> Int -> Int -> Int -> Int -> Int -> [Int]
swap [] _ _ _ _ _ = []
swap (x:xs) n minPlace fstVal scdVal iter
    | iter == n        = scdVal : swap xs n minPlace fstVal scdVal (iter + 1)
    | iter == minPlace = fstVal : swap xs n minPlace fstVal scdVal (iter + 1)
    | otherwise        = x : swap xs n minPlace fstVal scdVal (iter + 1)

--My TEST 4 6 1 8 9 8 10 5 4 9 10 3 2
--WHAT IT RETURNS: 3 4 6 1 8 9 8 10 5 4 9 10 2