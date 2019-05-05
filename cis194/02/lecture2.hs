-- 2nd ADTs --
data Thing = Shoe | Ship | SealingWax | Cabbage | King
 deriving Show


s1 :: Thing
s1 = Shoe

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King= False


-- Algebraic data types --
data FailableDouble = Failure | OK Double
 deriving Show

-- safe div --
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- extract---
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK x) = x

-- Person --
data Person = Person String Int Thing
 deriving Show

brent :: Person
brent = Person "Brent" 31 Shoe

stan :: Person
stan = Person "Stan" 28 King

-- Pattern matching custom type --
baz :: Person -> String
baz p@(Person n _ _) = "The name is " ++ (show p) ++ " is: " ++ n

-- Tree --
data Tree = Leaf Char | Node Tree Int Tree
 deriving Show

-- test --
t1 :: Tree
t1 = Node(Leaf 'x') 1 (Node (Leaf 'y') 10 (Leaf 'z'))
