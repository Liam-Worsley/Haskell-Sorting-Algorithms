module SortingAlgorithms.QuickSorter where


quickSort :: [Int] -> [Int]
quickSort lst = sortByPartition lst (middleVal lst)

middleVal :: [Int] -> Int
middleVal lst = length lst `div` 2

sortByPartition :: [Int] -> Int -> [Int]
sortByPartition [] _ = []
sortByPartition [x] _ = [x]
sortByPartition lst n = let x = lst !! n
                            newlist = splittingList lst x ([],[],[])
                            smaller = getLesser newlist
                            equal = getEqual newlist
                            greater = getGreater newlist
                                                            in sortByPartition smaller (middleVal smaller) ++ equal ++ sortByPartition greater (middleVal greater)


splittingList :: [Int] -> Int -> ([Int],[Int],[Int]) -> ([Int],[Int],[Int])
splittingList [] _ ans = ans
splittingList (x:xs) num (less, equal, greater)
    | x < num = let newlst = (less++[x],equal,greater) in splittingList xs num newlst
    | x == num = let newlst = (less,equal++[x],greater) in splittingList xs num newlst
    | otherwise = let newlst = (less,equal,greater++[x]) in splittingList xs num newlst

getLesser :: ([Int],[Int],[Int]) -> [Int]
getLesser (x,_,_) = x

getEqual :: ([Int],[Int],[Int]) -> [Int]
getEqual (_,x,_) = x

getGreater :: ([Int],[Int],[Int]) -> [Int]
getGreater (_,_,x) = x