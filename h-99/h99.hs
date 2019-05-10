import Data.List as List

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

-- | Problem 17: Split a list into two parts; the length of the first part is given
split :: [a] -> Int -> ([a], [a])
split a n = (getFirst a n, getLast a n)
    where
        getFirst [] n = []
        getFirst (x:xs) n
            | n == 1 = [x]
            | otherwise = x : getFirst xs (n - 1)
        getLast [] _ = []
        getLast (_:xs) n
            | n == 0 = xs
            | otherwise = getLast xs (n - 1)


-- | Problem 18: Extract a slice from a list. 
slice :: [a] -> Int -> Int -> [a]
slice (x:_) _ 1 = [x]
slice (x:xs) 1 k = x : slice xs 1 (k - 1) 
slice (x:xs) i k = slice xs (i - 1) (k - 1)

-- use built-in function
slice' :: [a] -> Int -> Int -> [a]
slice' a i k = take (k - i + 1) $ drop (i - 1) a

-- | Problem 19: Rotate a list N places to left
rotate :: [a] -> Int -> [a]
rotate s@(x:xs) n
    | n > 0 = rotate xs (n - 1) ++ [x]
    | n < 0 = rotate (last s : init s) (n + 1)
    | otherwise = s

-- | Problem 20: Remove the K'th element from a list. 
removeAt :: Int -> [a] -> (a, [a])
removeAt k a = (a !! (k - 1), take (k - 1) a ++ drop k a)

-- | Problem 21: Insert an element at a given position into a list. 
-- insertAt 'X' "abcd" 2 => "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt i a k = take (k - 1) a ++ [i] ++ drop (k - 1) a

-- recursion without helper (same idea behinde)
insertAt' :: a -> [a] -> Int -> [a]
insertAt' i a 1 = i : a
insertAt' i (x:xs) k = x : insertAt i xs (k - 1)

-- | Problem 22: Create a list containing all integers within a given range. 
-- range 4 9 => [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range x y = [x..y]

-- stupid complex
range' x y = take (y - x + 1) $ iterate (+1) x

-- | Problem 23: Extract a given number of randomly selected elements from a list. 
-- rnd_select "abcdefgh" 3 >>= putStrLn
-- eda
-- TODO

-- | Problem 27: Group the elements of a set into disjoint subsets

{- Problem 28: Sorting a list of lists according to length of sublists 
λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
["o","de","de","mn","abc","fgh","ijkl"]
-}
lsort :: Ord a => [[a]] -> [[a]]
lsort a = sortBy compareList a
    where
        compareList x y = compare (length x) (length y)

{- Problem 31: Determine whether a given integer number is prime
- isPrime 7 => True
-}
isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = not (diviable n (n - 1))
        where
            diviable _ 1 = False
            diviable n k =
                if n `mod` k == 0
                    then True
                    else diviable n (k - 1)

{- Problem 32: Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm. 
λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]
-}
myGCD :: Int -> Int -> Int
myGCD x 0 = abs x
myGCD x y = myGCD (abs y) (abs x `mod` abs y)

{- Problem 33: Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1. 
coprime 35 64 => True
    -}
coprime :: Int -> Int -> Bool
coprime x y = (myGCD x y) == 1

{- Problem 34: Calculate Euler's totient function phi(m). 
Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1. 
totient 10 => 4
-}

{- Problem 39: A list of prime numbers -}
primesR :: Int -> Int -> [Int]
primesR x y
    | x == y =
        if isPrime x
            then [x]
            else []
    | otherwise =
        if isPrime x
            then x : primesR (x + 1) y
            else primesR (x + 1) y

-- 99 questions/54A to 60

-- In Haskell, we can characterize binary trees with a datatype definition: 
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

{- Problem 96: Syntax checker 
λ> identifier "this-is-a-long-identifier"
True
λ> identifier "this-ends-in-"            
False
λ> identifier "two--hyphens" 
False
-}