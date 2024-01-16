module SortingAlgorithms.BubbleSorter where


--starts the bubble sort
bubbleSort :: [Int] -> [Int]
bubbleSort lst = bubbleRecursion lst (length lst - 1)

bubbleRecursion :: [Int] -> Int -> [Int]
bubbleRecursion lst 0 = lst
bubbleRecursion lst n = bubbleRecursion (runThrough lst 0 n) (n - 1)

runThrough :: [Int] -> Int -> Int -> [Int]
runThrough lst x n
  | x >= n = lst
  | goingToSwap lst x = runThrough (swap lst (lst !! x) (lst !! (x+1))) (x+1) n
  | otherwise = runThrough lst (x+1) n

--checks to see if you should swap the nth and n+1th element in the array
goingToSwap :: [Int] -> Int -> Bool
goingToSwap lst n = (lst !! n) > (lst !! (n+1))

--performs the swap
swap :: [Int] -> Int -> Int -> [Int]
swap lst a b = [if x==a then b else if x==b then a else x | x <- lst]