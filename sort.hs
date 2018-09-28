{-
Name: Sophia Trump
File: sort.hs
Desc: CS245, Assignment 2
-}

module Asst2 where

insert :: Ord a => a -> [a] -> [a] -- assumes the list is in non-decreasing order
insert n [] = [n]
insert n (x:xs)
 | n <= x = n : x : xs
 | otherwise = x : insert n (xs)


insertionSort :: Ord a => [a] -> [a] -- puts the list in non-decreasing order
insertionSort [] = []
insertionSort (x:xs) = insert x unSorted
  where
    unSorted = insertionSort xs  -- call insertionSort on the rest of the list


select :: Ord a => a -> [a] -> (a, [a])
select n [] = (n, [])
select n (x:xs)
 | mini < n = (mini, n : (orderList mini (x:xs)))
 | otherwise = (mini, orderList mini (x:xs))
  where
    mini = findMinElement n (x:xs)


findMinElement :: Ord a => a -> [a] -> a
findMinElement n [] = n
findMinElement n (x:xs)
 | x < n = findMinElement x xs
 | otherwise = findMinElement n xs


orderList :: Ord a => a -> [a] -> [a]
orderList n [] = []
orderList n (x:xs)
 | n == x = xs                               -- found the min element
 | otherwise = [x] ++ orderList n xs


selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort (x : xs) = [smallest] ++ selectionSort(leftOver)
  where
    (smallest, leftOver) = select x (xs)


merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys)
 | x < y = x : merge xs (y:ys) -- add x
 | x > y = y : merge (x:xs) ys -- add y
 | x == y = x : y : merge xs ys -- add x and y


-- SOURCE : from textbook Hu page 38
split :: [a] -> ([a],[a])
split xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort n
 | (length n) > 1 = merge (mergeSort splitLeft) (mergeSort splitRight) -- split until you get down to one
 | otherwise = n  -- the length of n is 1, so return n
 where
   (splitLeft, splitRight) = split n


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort(smaller) ++ [x] ++ quickSort(larger)
  where
    (smaller, larger) = subSortQuick x xs


subSortQuick :: Ord a => a -> [a] -> ([a],[a])
subSortQuick _ [] = ([],[])
subSortQuick n (x:xs)
 | x < n =  (x : leftList, rightList)-- put left of the pivot
 | otherwise =  (leftList, x: rightList) -- put right of the pivot
  where
    (leftList,rightList) = subSortQuick n xs -- recurse until entire list is divided by sublist
