module SortingAlgorithms.QuickSorter where

--starts the quick sort function
quickSort :: [Int] -> [Int]
quickSort lst = sortByPartition lst (middleVal lst)

--splits continuously splits up the list into less than, equal to, and greater than and then recursively sorts those sublists
sortByPartition :: [Int] -> Int -> [Int]
sortByPartition [] _ = []
sortByPartition [x] _ = [x]
sortByPartition lst n = let x = lst !! n
                            newlist = splittingList lst x ([],[],[])
                            smaller = getLesser newlist
                            equal = getEqual newlist
                            greater = getGreater newlist
                                                            in sortByPartition smaller (middleVal smaller) ++ equal ++ sortByPartition greater (middleVal greater)


--splits the list up into a tuple of 3 different lists, those with lesser, equal, and greater values than the value we wanted to partition for
splittingList :: [Int] -> Int -> ([Int],[Int],[Int]) -> ([Int],[Int],[Int])
splittingList [] _ ans = ans
splittingList (x:xs) num (less, equal, greater)
    | x < num = let newlst = (less++[x],equal,greater) in splittingList xs num newlst
    | x == num = let newlst = (less,equal++[x],greater) in splittingList xs num newlst
    | otherwise = let newlst = (less,equal,greater++[x]) in splittingList xs num newlst


--SIMPLE HELPER FUNCTIONS

--gets the index of the middle of the list
middleVal :: [Int] -> Int
middleVal lst = length lst `div` 2

--unwraps a 3-tuple for its first element
getLesser :: ([Int],[Int],[Int]) -> [Int]
getLesser (x,_,_) = x

--unwraps a 3-tuple for its second element
getEqual :: ([Int],[Int],[Int]) -> [Int]
getEqual (_,x,_) = x

--unwraps a 3-tuple for its third element
getGreater :: ([Int],[Int],[Int]) -> [Int]
getGreater (_,_,x) = x