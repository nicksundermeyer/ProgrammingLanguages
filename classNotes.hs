import Data.Maybe

-- 10/1

-- Setting up environment
createEnv = []
emptyEnv = []

extendEnv :: (String, Integer) -> [(String, Integer)] -> [(String, Integer)]
extendEnv (a,b) [] = [(a,b)]
extendEnv (a,b) (bind:binds) = (a,b):(bind:binds)

applyEnv :: String -> [(String,Integer)] -> Integer
applyEnv name [] = error "Variable not found"
applyEnv name ((n,value):binds) = if name == n
                                    then value
                                    else applyEnv name binds

-- applyEnv "x" (extendEnv ("x",5) emptyEnv)  ->  5

-- 9/26

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum_curried :: (Num a) => [a] -> a
sum_curried = foldl (+) 0;

-- Foldl definition
-- goes through a list and applies a function to each element, stores values in accumulator, returns acc at the end
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

-- Output examples
-- *Main> foldl (+) 0 [1,2,3]
-- 6
-- *Main> foldl (/) 3 []
-- 3.0
-- *Main> foldl (/) 64 [4,2,4]
-- 2.0
-- *Main> foldl (*) 1 [3,2,1]
-- 6
-- *Main>

-- Definition of foldr
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

-- Definition of map
map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map f xs

-- Definition of map using foldr
-- Takes a function, gives out a list
map_foldr :: (a->b) -> [a] -> [b]
map_foldr f xs = foldr (\x acc -> f x : acc) [] xs -- apply function to each element of list with foldr

-- Definition of map using foldl
map_foldl :: (a->b) -> [a] -> [b]
map_foldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- Pattern matching example
data Suit = Club | Diamond | Heart | Spade
  deriving (Show, Eq, Enum) -- Defining necessary type classes
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Show, Ord, Enum) -- Eq = able to compare for equality, Show = printable, Ord = orderable
data Card = Card (Rank) (Suit)
  deriving (Show, Eq)

type Deck = [Card]
deck :: Deck
deck = [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]]

-- Finding color of card
data Color = Red | Black
  deriving (Show, Eq)

color :: Card -> Color
color (Card _ su) = if(su == Club || su == Spade)
                        then Black
                    else Red

-- find if cards are alternating colors
alternateColors :: Card -> Card -> Bool
alternateColors a b = color a /= color b

-- Function to check if the second card follow the first
follows :: Card -> Card -> Bool
follows (Card Ace _) _ = False -- Base case, nothing follows ace since ace is high
follows (Card a _) (Card b _) = if (b > a) then True else False -- Compare ranks

-- Function check for pair of cards
findPair [] = Nothing
findPair (x:rest) = if length(filter (==x) rest) > 0
                        then Just x
                    else findPair rest

-- findPair using foldl
findPair' (x:rest) = if (foldl (\acc card -> (x==card) || acc) False rest)
                        then Just x
                    else findPair' rest

-- 9/24

-- partial application of function
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9

multByTen :: (Num a) => a -> a
multByTen = (*10)

subtractTen :: (Num a) => a -> a
subtractTen = subtract 10

applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)

-- zipwith implementation
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : map f xs

-- 9/19

-- binary tree
-- Test case: MyNode 1 (MyNode 2 Null Null) (MyNode 3 Null Null)
data MyTree a = Null | MyNode (MyTree a) a (MyTree a)
    deriving Show -- allows us to print tree to see what it looks like

toList :: MyTree a -> [a]
toList Null = []
toList (MyNode l a r) = toList l ++ [a] ++ toList r

isEmpty :: MyTree a -> Bool
isEmpty Null = True
isEmpty _ = False

-- checking if MyTree contains a value
contains :: Ord a => MyTree a -> a -> Bool
contains Null _ = False
contains (MyNode left value right) x
    | x == value = True
    | x < value = contains left x
    | otherwise = contains right x

-- can't modify existing tree, so need to build new parts of the tree when inserting or removing
insert :: Ord a => MyTree a -> a -> MyTree a
insert Null x = MyNode Null x Null -- insert into empty tree
insert (MyNode left value right) x
    | value < x = MyNode left value (insert right x) -- need to rebuild entire pathway to node, can't just change the single value
    | otherwise = MyNode (insert left x) value right

-- inorder traversal, put all values in order into a list
inorder :: Ord a => MyTree a -> [a]
inorder Null = []
inorder (MyNode left value right) = inorder left ++ [value] ++ inorder right

doubleTree :: Num a => MyTree a -> MyTree a
doubleTree Null = Null
doubleTree (MyNode left value right) =
    (MyNode (doubleTree left) (2*value) (doubleTree right))

-- DOESN'T WORK
-- quicksort :: Ord a -> [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) =  -- (x:xs) means first element followed by more elements
-- -- this finds a list of all elements a such that a is from list xs, and a <= x
-- -- called list filtering
--     let smaller = [a | a <- xs, a <= x]
--         larger = [a | a <- xs, a > x]
--     in
--         quicksort smaller ++ [x] ++ quicksort larger

-- a input, String output of function
percentToGrade :: (Num a, Ord a) => a -> String
-- boolean conditions, acts similar to switch statement
percentToGrade percent
    | percent <= 60 = "F"
    | percent <= 70 = "C"
    | percent <= 80 = "B"
    | percent <= 90 = "A"
    | otherwise = "A+"

-- type must be possible to order (compare) because of Ord
-- takes in two parameters one after the other, puts out a bool
myCompare :: (Ord a) => a -> a -> Bool
myCompare x y
    | x < y = True
    | otherwise = False

initials :: String -> String -> String
initials firstname lastname = [f] ++ " " ++ [l]
    where (f:_) = firstname -- binding pattern f:_ to String firstname
          (l:_) = lastname

-- multThree :: (Num a) => a -> a -> a -> a
-- multThree x y z = x * y * z

-- -- same thing as multThree, but first argument is always 9
-- multBy9 :: (Num a) => a -> a -> a
-- multBy9 = multThree 9

-- 9/17

-- Maybe example
-- helper function to check if x has a value, or is nothing
valOf x = case x of
    (Just value) -> value
    Nothing -> error "No value is available"

findmax lst = 
    if null lst
        then Nothing
        else 
            let result = findmax (tail lst) -- recursively call findmax
            in
                if isJust result && (valOf result > head lst) -- check if result of findmax is > current head of list, and also check whether it is Nothing with isJust
                    then result
                    else Just (head lst)

-- let example
countup n = 
    -- using let to bind temporary local function
    let count start end = 
            if start == end 
            then [end]
            else start : count (start + 1) end
    in
        count 1 n

-- 9/12

myMinimum a b = if a < b then a else b
myMinimumTwo (a,b) = if a < b then a else b

myAdd (a,b,c) (d,e,f) = (a+d, b+e, c+f)

addList xs = 
    if null xs then 0
    else head xs + addList (tail xs)

doubleList xs =
    if null xs then [] 
    else 2 * head xs : doubleList (tail xs) -- : adds to beginning of list
-- [1,2,3] = 1:2:3:[]

-- factorial version one
factorial n = 
    if n == 0
        then 1
        else n * factorial (n-1)

-- factorial version 2
-- pattern matching, does one thing if it recognizes 0, another if anything else
factorial' 0 = 1
factorial' n = n * factorial' (n-1)

-- factorial version 3
factorial'' n = product[1..n]