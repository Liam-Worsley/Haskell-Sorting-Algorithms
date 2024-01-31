# Haskell-Sorting-Algorithms
Finished as of 1/31/2024
This is a personal project that does a bunch of sorting algorithms in haskell.

List of Algorithms: Bubble, Insertion, Merge, Selection, Quick, Thanos, Stalin

To run the program, make sure your computer can run haskell then do the following commands to get the help menu so you can run individual or all the sorts:
make
runhaskell Main.hs -h

The following are a list of flaws with this program:
1. Uses "!!" in many of the sorts, which has the potential to cause problems (seems not to thankfully).
2. The sorts only deal with integers and the files must have only 1 array in them with numbers separated by a space.
3. Some of the sorts are not as optimized as they would be in other languages. THis mainly applies to bubble sort which is O(n^3) instead of O(n^2) because whenever you swap 2 elements you must create a new list.
4. Two of the sorts, Thanos and Stalin, are what the young kids call "meme sorts" and they will remove elements from their arrays in order to achieve a sorted array. These ones are just for fun and should never be used.

I am fully happy with people copying/stealing/remaking this code, this is a public project that has been done by many others. HOWEVER for students, try to solve it on your own first, if I struggled with this then so must you :)
