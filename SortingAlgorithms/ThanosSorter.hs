module SortingAlgorithms.ThanosSorter where
import System.Random
import System.IO.Unsafe


randomNumber :: IO Int
randomNumber = getStdRandom (randomR (1,2))

thanosSort :: [Int] -> [Int]
thanosSort lst = if isSorted lst then lst else
                 let newRand = randomNumber
                     rand2 = (unsafePerformIO newRand)
                     newlst = destroyHalf lst rand2 0
                 in thanosSort newlst
                     
destroyHalf :: [Int] -> Int -> Int -> [Int]
destroyHalf [] _ _ = []
destroyHalf (x:xs) 1 iter = if iter `mod` 2 == 0 then destroyHalf xs 1 (iter+1) else x:destroyHalf xs 1 (iter+1)
destroyHalf (x:xs) 2 iter = if iter `mod` 2 == 0 then x:destroyHalf xs 1 (iter+1) else destroyHalf xs 1 (iter+1)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x < y then isSorted (y:xs) else False