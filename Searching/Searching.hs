module Searching
(
linearSearch,
linearSearch',
binarySearch,
) where

{-
Linear Search: takes an element to search and a list to search in and returns True if
the element is in the list or False otherwise.The function works just like the 
in-built "elem" function in Data.List module. 
-}
linearSearch :: (Eq a,Ord a) => a -> [a] -> Bool
linearSearch _ [] = False
linearSearch a (x:xs)
    | a == x    = True
    | otherwise = linearSearch a xs

{-
Linear Search': Similar to the above function, just this one is implemented with
foldl
-}
linearSearch' :: (Eq a,Ord a) => a -> [a] -> Bool
linearSearch' a arr = foldl (\acc x -> if x == a then True else acc) False arr


{-
Binary Search: takes an element and a sorted list and returns True if the element
is in the list or False otherwise.
-}
binarySearch :: (Eq a,Ord a) => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch a arr
    | (arr !! mid) == a     = True
    | (arr !! mid) < a      = binarySearch a $ drop (mid+1) arr
    | otherwise             = binarySearch a $ take mid arr
    where mid = div (length arr) 2