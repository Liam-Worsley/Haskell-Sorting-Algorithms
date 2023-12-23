{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import System.Console.GetOpt
import SortingAlgorithms.BubbleSorter
import SortingAlgorithms.InsertionSorter
import SortingAlgorithms.MergeSorter
import SortingAlgorithms.QuickSorter
import SortingAlgorithms.SelectionSorter


--different flags for the sorting algorithms
data Flag = Help | Bubble | Insertion | Quick | Selection | Merge | All deriving (Show, Eq)

--options for the help menu
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Will give you this help menu. As of now, all algorithms will only sort integers."
          , Option ['b'] ["bubble"] (NoArg Bubble) "Will run a bubble sort in O(n^2)."
          , Option ['i'] ["insertion"] (NoArg Insertion) "Will run a insertion sort in O(n^2)."
          , Option ['q'] ["quick"] (NoArg Quick) "Will run a quick sort in O(nlogn)."
          , Option ['s'] ["selection"] (NoArg Selection) "Will run a selection sort in O(n^2)."
          , Option ['m'] ["merge"] (NoArg Merge) "Will run a merge sort in O(nlogn)."
          , Option ['a'] ["all"] (NoArg All) "Will run all of the sorts."
          ]

--main functions, will run all requested sorting algorithms on the file you want
main :: IO ()
main = do
            args <- getArgs
            let (flags, inputs, errors) = getOpt Permute options args
            if Help `elem` flags || not (null errors)
            then putStrLn $ usageInfo "./DotsAndBoxes [options] [filename]\nOptions:" options
            else do 
                    let fname = if null inputs then "arrayFiles/array.txt" else head inputs
                    contents <- readFile fname
                    let listOfStrs = splitOn " " contents
                        listOfInts = map read listOfStrs
                    showArray listOfInts
                    if All `elem` flags then doAllFlags listOfInts else do doRequestedFlags flags listOfInts

--displays the array
showArray :: [Int] -> IO ()
showArray [] = do putStrLn ""
showArray (x:xs) = do putStr (show x)
                      putStr " "
                      showArray xs

--does all of the sorting algorithms
doAllFlags :: [Int] -> IO ()
doAllFlags lst = do putStr "Bubble Sort: "
                    showArray (bubbleSort lst 0)
                    putStr "Insertion Sort: "
                    showArray (insertionSort lst 0)
                    putStr "Quick Sort: "
                    showArray (quickSort lst 0)
                    putStr "Selection Sort: "
                    showArray (selectionSort lst 0)
                    putStr "Merge Sort: "
                    showArray (mergeSort lst 0)

--finds the requested sorting algorithms and does them
doRequestedFlags :: [Flag] -> [Int] -> IO ()
doRequestedFlags [] _ =                 do putStrLn ""
doRequestedFlags (Bubble:xs) lst =      do putStr "Bubble Sort: "
                                           showArray (bubbleSort lst 0)
                                           doRequestedFlags xs lst
doRequestedFlags (Insertion:xs) lst =   do putStr "Insertion Sort: "
                                           showArray (insertionSort lst 0)
                                           doRequestedFlags xs lst
doRequestedFlags (Merge:xs) lst =       do putStr "Merge Sort: "
                                           showArray (mergeSort lst 0)
                                           doRequestedFlags xs lst
doRequestedFlags (Quick:xs) lst =       do putStr "Quick Sort: "
                                           showArray (quickSort lst 0)
                                           doRequestedFlags xs lst
doRequestedFlags (Selection:xs) lst =   do putStr "Selection Sort: "
                                           showArray (selectionSort lst 0)
                                           doRequestedFlags xs lst