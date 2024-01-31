module SortingAlgorithms.InsertionSorter where



--starts the iterator to go up the array
insertionSort :: [Int] -> [Int]
insertionSort lst = doIterator lst 0

--checks each pair and will move numbers back down when necessary
doIterator :: [Int] -> Int -> [Int]
doIterator lst n = if n >= ((length lst) - 1) then lst else let newlst = goingDown lst n in doIterator newlst (n+1)

--checks to switch and switches variables
goingDown :: [Int] -> Int -> [Int]
goingDown lst n = if n > (-1) && goingToSwap lst n then let newlst = swap lst n 0 in goingDown newlst (n-1) else lst

--checks to see if you should swap the nth and n+1th element in the array
goingToSwap :: [Int] -> Int -> Bool
goingToSwap lst n = (lst !! n) > (lst !! (n+1))

--swaps the ath and a+1th element in the array
swap :: [Int] -> Int -> Int -> [Int]
swap [] _ _ = []
swap [x] _ _ = [x]
swap (x:y:xs) a b = if a /= b then [x] ++ swap (y:xs) a (b+1) else y:x:xs 