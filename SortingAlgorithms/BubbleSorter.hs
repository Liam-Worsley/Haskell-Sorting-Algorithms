module SortingAlgorithms.BubbleSorter where

--main function, starts the firstForLoop function
bubbleSort :: [Int] -> [Int]
bubbleSort lst = firstForLoop lst (length lst - 1)

--outer for loop that continually decreases the size of checking
firstForLoop :: [Int] -> Int -> [Int]
firstForLoop lst 0 = lst
firstForLoop lst n = firstForLoop (secondForLoop lst 0 n) (n - 1)

--inner for loop that runs through the array and checks for swaps
secondForLoop :: [Int] -> Int -> Int -> [Int]
secondForLoop lst x n = if x >= n then lst else if goingToSwap lst x then let newlst = swap lst x 0 in secondForLoop newlst (x+1) n else secondForLoop lst (x+1) n

--checks to see if you should swap the nth and n+1th element in the array
goingToSwap :: [Int] -> Int -> Bool
goingToSwap lst n = (lst !! n) > (lst !! (n+1))

--swaps the ath and a+1th element in the array
swap :: [Int] -> Int -> Int -> [Int]
swap [] a b = []
swap [x] a b = [x]
swap (x:y:xs) a b = if a /= b then [x] ++ swap (y:xs) a (b+1) else y:x:xs 