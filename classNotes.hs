import Data.Maybe

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
data MyTree a = EmptyTree | MyNode (MyTree a) a (MyTree a)
    deriving Show -- allows us to print tree to see what it looks like

isEmpty :: MyTree a -> Bool
isEmpty EmptyTree = True
isEmpty _ = False

-- checking if MyTree contains a value
contains :: Ord a => MyTree a -> a -> Bool
contains EmptyTree _ = False
contains (MyNode left value right) x
    | x == value = True
    | x < value = contains left x
    | otherwise = contains right x

-- can't modify existing tree, so need to build new parts of the tree when inserting or removing
insert :: Ord a => MyTree a -> a -> MyTree a
insert EmptyTree x = MyNode EmptyTree x EmptyTree -- insert into empty tree
insert (MyNode left value right) x
    | value < x = MyNode left value (insert right x) -- need to rebuild entire pathway to node, can't just change the single value
    | otherwise = MyNode (insert left x) value right

-- inorder traversal, put all values in order into a list
inorder :: Ord a => MyTree a -> [a]
inorder EmptyTree = []
inorder (MyNode left value right) = inorder left ++ [value] ++ inorder right

doubleTree :: Num a => MyTree a -> MyTree a
doubleTree EmptyTree = EmptyTree
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