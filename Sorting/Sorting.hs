module Sorting
(
quickSort,
mergeSort,
insertionSort
) where

import Data.List

{-
Quick Sort: takes a list and splits it into two parts:
lesserArray, the one containing the elements less than or equal to some pivot
biggerArray, the one containing the elements greater than the pivot.
It then sorts the two parts recursively till the whole list is sorted
-}
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = let lesserArray = filter (<=x) xs; biggerArray = filter (>x) xs
                    in quickSort lesserArray ++ [x] ++ quickSort biggerArray

-- Merge: takes two sorted lists and merges them into one sorted list.
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys) 
    | x <= y    = x : merge xs b
    | otherwise = y : merge a ys

{-
MergeSort: takes a list and splits it into two lists:
the left part, the one contanining the elements left of the mid point
the right part, the one containing the elements right of the mid point.
It sorts the two parts recursively and then merges them to get the sorted list
-}

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = [x]
mergeSort a = merge (mergeSort $ take mid a) (mergeSort $ drop mid a)
                    where mid = div (length a) 2


{-
Insertion Sort: Normally Insertion Sort works by iterating the list from left to right 
and keep placing each element into its rightful place in the already sorted list till the entire list is sorted.
In this case I have done the opposite, rather than placing the elements from left to right,
here the elements get placed from right to left.
-}                    
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x $ insertionSort xs