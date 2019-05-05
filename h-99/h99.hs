-- | Extract the last element from a list, which must be finite and non-empty
myLast :: [a] -> a
myLast [] = errorWithoutStackTrace "Empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast(xs)

-- | Problem 2: find the last but one element of the list
myButLast :: [a] -> a
myButLast [] = errorWithoutStackTrace "Empty list"
myButLast (x:[]) = errorWithoutStackTrace "Only one elem"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast(xs)

-- | Problem 3: Find the K'th element of the list (1-based list)
elementAt :: [a] -> Int -> a
elementAt [] _ = errorWithoutStackTrace "Out of index"
elementAt (x:_) 1 = x
elementAt (x:xs) k = elementAt xs (k - 1)

-- | Problem 4: Find the the number of elements
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

-- | Prelude functions used
myLength1 :: [a] -> Int
myLength1 = sum . map (\_ -> 1)

-- | Problem 5: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse(xs) ++ [x]


--- | Problem 6: Find out a listif a palin drome
isPalindrome :: Ord a => [a] -> Bool
isPalindrome [] = errorWithoutStackTrace "Empty list"
isPalindrome (x:[]) = True
isPalindrome (x:xs) = x < (head xs) && isPalindrome(xs)

-- | Problem 7: Flatten a nested list structure
-- ignore

-- | Problem 8: Eliminate consecutive duplicates of list elements. 
compress :: Eq a => [a] -> [a]
compress a = compressSub a []
 where compressSub (x:xs) list = 
        if elem x list
            then compressSub xs list  
            else compressSub xs (list ++ [x])
       compressSub [] list = list

compress1 :: Eq a => [a] -> [a]
compress1 (x:xs) = x : (compress $ dropWhile (== x) xs)

-- | Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
-- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] => ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack a = packSub a []
 where packSub [] list = list
       packSub (x:xs) list = packSub xs (addToPack x list)
        where addToPack n [] = [[n]]
              addToPack n (lx:lxs) =
               if (n == (head lx))
                then (n : lx) : lxs
                else lx : (addToPack n lxs)

pack1 :: (Eq a) => [a] -> [[a]]
pack1 [] = []
pack1 [x] = [[x]]
pack1 (x:xs) = if x `elem` (head (pack1 xs))
              then (x:(head (pack1 xs))):(tail (pack1 xs))
              else [x]:(pack1 xs)

-- | Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1, x)]
encode (x:xs) = 
    if x == (snd $ head $ encode xs)
        then ((fst $ head $ encode xs) + 1, x) : (tail $ encode xs)
        else (1, x) : (encode xs)