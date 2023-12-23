module SortingAlgorithms.BubbleSorter where


--starts the bubble sort
bubbleSort :: [Int] -> Int -> [Int]
bubbleSort lst n
  | length lst-1 == n = lst
  | goingToSwap lst n = let newlst = shouldSwap lst n in bubbleSort newlst 0
  | otherwise = let newlst = shouldSwap lst n in bubbleSort newlst (n+1)

--checks to see if you should swap the nth and n+1th element in the array
goingToSwap :: [Int] -> Int -> Bool
goingToSwap lst n = (lst !! n) > (lst !! (n+1))

--actually calls the swap method and returns the new array with the swapped elements
shouldSwap :: [Int] -> Int -> [Int]
shouldSwap lst n = if goingToSwap lst n then swap lst (lst !! n) (lst !! (n+1)) else lst

--performs the swap
swap :: [Int] -> Int -> Int -> [Int]
swap lst a b = [if x==a then b else if x==b then a else x | x <- lst]

