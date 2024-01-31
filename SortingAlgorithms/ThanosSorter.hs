module SortingAlgorithms.ThanosSorter where
import System.Random
import System.IO.Unsafe

--recursively calls itself until the array is sorted, if not it destroys half and checks again
thanosSort :: [Int] -> [Int]
thanosSort lst = if isSorted lst then lst else
                 let newRand = randomNumber
                     rand2 = (unsafePerformIO newRand)
                     newlst = destroyHalf lst rand2 0
                 in thanosSort newlst

--random number generator that returns 1 or 2
randomNumber :: IO Int
randomNumber = getStdRandom (randomR (1,2))

--removes every other element of the array, does even or odd based off the random number
destroyHalf :: [Int] -> Int -> Int -> [Int]
destroyHalf [] _ _ = []
destroyHalf (x:xs) 1 iter = if iter `mod` 2 == 0 then destroyHalf xs 1 (iter+1) else x:destroyHalf xs 1 (iter+1)
destroyHalf (x:xs) 2 iter = if iter `mod` 2 == 0 then x:destroyHalf xs 1 (iter+1) else destroyHalf xs 1 (iter+1)

--checks to see if the list is sorted
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x < y then isSorted (y:xs) else False