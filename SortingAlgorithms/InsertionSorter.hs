module SortingAlgorithms.InsertionSorter where




insertionSort :: [Int] -> Int -> [Int]
insertionSort lst n = if length lst < n then lst
                                         else let newlst = insertAt lst n in insertionSort newlst (n+1)

insertAt :: [Int] -> Int -> [Int]
insertAt lst n = if isBiggest lst n then lst else whereToPlace lst n (n-1)

isBiggest :: [Int] -> Int -> Bool
isBiggest [] _ = True
isBiggest lst@(x:xs) n
  | x > (lst !! n) = False
  | x == (lst !! n) = True
  | otherwise = isBiggest xs n

whereToPlace :: [Int] -> Int -> Int -> [Int]
whereToPlace lst n 0 = placeNum lst n 0 0
whereToPlace lst n x = if lst !! n < lst !! x then whereToPlace lst n (x-1) else placeNum lst n x 0

placeNum :: [Int] -> Int -> Int -> Int -> [Int]
placeNum lst@(x:xs) n small iter = if iter < small then x:placeNum xs n small (iter+1) else (lst !! n):x:restOfArray xs (lst !! n)

restOfArray :: [Int] -> Int -> [Int]
restOfArray lst n = [x | x <- lst, x /= n]