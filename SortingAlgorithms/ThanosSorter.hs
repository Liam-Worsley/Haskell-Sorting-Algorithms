module SortingAlgorithms.ThanosSorter where
import System.Random


thanosSort :: [Int] -> [Int]
thanosSort lst = lst
{-

randomNumber :: IO Int
randomNumber = getStdRandom (randomR (1,2))

thanosSort :: [Int] -> [Int]
thanosSort lst = do newRand <- randomNumber
                    let rnum = newRand
                        ans = if rnum == 1
                              then (runThroughOdd lst 0)
                              else (runThroughEven lst 0)
                    ans

runThroughEven :: [Int] -> Int -> [Int]
runThroughEven lst x = lst

runThroughOdd :: [Int] -> Int -> [Int]
runThroughOdd lst x = lst
-}