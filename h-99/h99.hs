-- | Extract the last element from a list, which must be finite and non-empty
myLast :: [a] -> a
myLast [] = errorWithoutStackTrace "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- | Problem 2: find the last but one element of the list
myButLast :: [a] -> a
myButLast [] = errorWithoutStackTrace "Empty list"
myButLast [_] = errorWithoutStackTrace "Only one elem"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

-- | Problem 3: Find the K'th element of the list (1-based list)
elementAt :: [a] -> Int -> a
elementAt [] _ = errorWithoutStackTrace "Out of index"
elementAt (x:_) 1 = x
elementAt (x:xs) k = elementAt xs (k - 1)

-- | Problem 4: Find the the number of elements
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- | Prelude functions used
myLength1 :: [a] -> Int
myLength1 = sum . map (const 1)

-- | Problem 5: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]


--- | Problem 6: Find out a listif a palin drome
isPalindrome :: Ord a => [a] -> Bool
isPalindrome [] = errorWithoutStackTrace "Empty list"
isPalindrome [x] = True
isPalindrome (x:xs) = x < head xs && isPalindrome xs

-- | Problem 7: Flatten a nested list structure
-- ignore

-- | Problem 8: Eliminate consecutive duplicates of list elements. 
compress :: Eq a => [a] -> [a]
compress a = compressSub a []
 where compressSub (x:xs) list = 
        if x `elem` list
            then compressSub xs list  
            else compressSub xs (list ++ [x])
       compressSub [] list = list

compress1 :: Eq a => [a] -> [a]
compress1 (x:xs) = x : compress ( dropWhile (== x) xs)

-- | Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
-- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] => ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack a = packSub a []
 where packSub [] list = list
       packSub (x:xs) list = packSub xs (addToPack x list)
        where addToPack n [] = [[n]]
              addToPack n (lx:lxs) =
               if n == head lx
                then (n : lx) : lxs
                else lx : addToPack n lxs

pack1 :: (Eq a) => [a] -> [[a]]
pack1 [] = []
pack1 [x] = [[x]]
pack1 (x:xs) = if x `elem` head (pack1 xs)
              then (x:(head (pack1 xs))) : (tail (pack1 xs))
              else [x]:(pack1 xs)

-- | Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1, x)]
encode (x:xs) = 
    if x == snd (head $ encode xs)
        then (fst (head (encode xs)) + 1, x) : tail (encode xs)
        else encode [x] ++ encode xs

data ListItem a = Mutiple Int a | Single a
 deriving (Show)

-- | Problem 11: Modified run-length encoding
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
 where
    encodeHelper (1,x) = Single x
    encodeHelper (n, x) = Mutiple n x


-- Problem 12: Decode a run-length encoded list
decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeHelper x ++ decodeModified xs
 where
    decodeHelper (Single x) = [x]
    decodeHelper (Mutiple n x) = replicate n x 

decodeModified' :: (Eq a) => [ListItem a] -> [a]
decodeModified' = concatMap decodeHelper
 where
    decodeHelper (Single x) = [x]
    decodeHelper (Mutiple n x) = replicate n x

-- Problem 13: Run-length encoding of a list (direct solution). 

-- | Problem 14: Duplicate the elements of the list
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

-- Raw solution
dupli' :: [a] -> [a]
dupli' [] = []
dupli' (x:xs) = x : x : dupli' xs

-- | Problem 15: Replicate the elements of a list a given number of times. 
repli :: [a] -> Int -> [a]
repli a n = concatMap (rephelper n) a
 where 
    rephelper 1 x = [x]
    rephelper n x = x : rephelper (n -1) x 

-- Raw solution
repli' :: [a] -> Int -> [a]
repli' [] _ = []
repli' (x:xs) n = rephelper n x ++ repli' xs n
 where
    rephelper 1 x = [x]
    rephelper n x = x : rephelper (n -1) x 

-- | Problem 16: Drop every N'th element from a list. 
dropEvery :: [a] -> Int -> [a]
dropEvery a n = dropHelper a n 1
    where
        dropHelper [] _ _ = []
        dropHelper (x:xs) n k
            | k == n = dropHelper xs n 1
            | otherwise = x : dropHelper xs n (k + 1)