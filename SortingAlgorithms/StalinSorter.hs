module SortingAlgorithms.StalinSorter where

--goes through the element once, and removes all elements that are smaller than the previous element
--NOT A GOOD SORT THIS DELETES LOTS OF ELEMENTS
--but it is funny :)
stalinSort :: [Int] -> [Int]
stalinSort [] = []
stalinSort [x] = [x]
stalinSort (x:y:xs) = if x <= y then x : stalinSort (y:xs) else stalinSort (x:xs)