module SortingAlgorithms.QuickSorter where


quickSort :: [Int] -> [Int]
quickSort lst = sortByPartition lst (middleVal lst)

middleVal :: [Int] -> Int
middleVal lst = length lst `div` 2

sortByPartition :: [Int] -> Int -> [Int]
sortByPartition [] _ = []
sortByPartition [x] _ = [x]
sortByPartition lst n = let x = lst !! n
                            bigger = findAllBigger lst x
                            smaller = findAllSmaller lst x
                            howManyDups = findAllDups lst x 0
                            makeTheDupList = makeDupList x howManyDups
                                                            in sortByPartition smaller (middleVal smaller) ++ makeTheDupList ++ sortByPartition bigger (middleVal bigger)
findAllBigger :: [Int] -> Int -> [Int]
findAllBigger lst n = [x | x <- lst, x > n]

findAllSmaller :: [Int] -> Int -> [Int]
findAllSmaller lst n = [x | x <- lst, x < n]

findAllDups :: [Int] -> Int -> Int -> Int
findAllDups [] x n = n
findAllDups (x:xs) num n = if x == n then findAllDups xs num (n+1) else findAllDups xs num n

makeDupList :: Int -> Int -> [Int]
makeDupList x 0 = []
makeDupList x n = x : makeDupList x (n-1)