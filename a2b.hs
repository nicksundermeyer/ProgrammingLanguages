module A2b where

-- 1. removeAllExcept - takes an element and a list, and returns a list with all the elements other than the one given removed
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept _ [] = []
removeAllExcept element list
    | head list == element = element : removeAllExcept element (tail list)
    | otherwise = removeAllExcept element (tail list)

-- 2. removeAll - takes an element and a list, and returns the list with all occurences of that element removed
removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll element list
    | head list /= element = head list : removeAll element (tail list)
    | otherwise = removeAll element (tail list)

-- 3. substitute - takes two elements and a list, and returns a list with every instance of the first element replaced with the second
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y list
    | head list == x = y : substitute x y (tail list)
    | otherwise = (head list) : substitute x y (tail list)

-- mergeTwo helper function, merges two lists together into one
mergeTwo :: Ord a => [a] -> [a] -> [a]
mergeTwo x [] = x
mergeTwo [] y = y
mergeTwo (x:xs) (y:ys)
    | x < y = x:(mergeTwo xs (y:ys))
    | otherwise = y:(mergeTwo (x:xs) ys)

-- 4. mergeSorted3 - takes 3 lists in sorted order and merges them into a single list, also in increasing order (using helper function mergeTwo)
mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 (x:xs) (y:ys) (z:zs) = mergeTwo (mergeTwo (x:xs) (y:ys)) (z:zs)

-- TriTree - trinary tree data structure. EmptyNode is an empty node, and TriNode is a node holding an element/value with three branches, each a trinary tree
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)
    deriving Show

-- toList helper function - takes a trinary tree and returns a list containing all the elements (in preorder)
toList :: TriTree a -> [a]
toList EmptyNode = []
toList (TriNode a l m r) = [a] ++ toList l ++ toList m ++ toList r

-- 1. nodeValue - takes a trinary tree and returns the value of the given node
nodeValue :: TriTree a -> a
nodeValue (TriNode a l m r) = a

-- 2. leftChild - takes a trinary tree and returns the left child, or an error if the tree is empty
leftChild :: TriTree a -> TriTree a
leftChild EmptyNode = error "Empty Tree"
leftChild (TriNode a l m r) = l

-- 3. middleChild - takes a trinary tree and returns the middle child, or an error if the tree is empty
middleChild :: TriTree a -> TriTree a
middleChild EmptyNode = error "Empty Tree"
middleChild (TriNode a l m r) = m

-- 4. rightChild - takes a trinary tree and returns the right child, or an error if the tree is empty
rightChild :: TriTree a -> TriTree a
rightChild  EmptyNode = error "Empty Tree"
rightChild  (TriNode a l m r) = r

-- 5. inTree - takes an element and a trinary tree, and returns True or False whether the element is in the given tree
inTree :: Eq a => a -> TriTree a -> Bool
inTree _ EmptyNode = False
inTree x (TriNode a l m r)
    | x == a = True
    | otherwise = (inTree x l) || (inTree x m) || (inTree x r)

-- 6. leafList - takes a trinary tree and returns a list of all the leaves of the tree (nodes with three empty branches)
leafList :: TriTree a -> [a]
leafList (TriNode value EmptyNode EmptyNode EmptyNode) = [value]
leafList (TriNode value l m r) = (leafList l) ++ (leafList m) ++ (leafList r)
leafList _ = []

-- 6. inOrderMap - takes a function and a trinary tree, and returns a trinary tree with the function applied to every element of the original tree
inOrderMap :: (a -> b) -> TriTree a -> TriTree b
inOrderMap _ EmptyNode = EmptyNode
inOrderMap f (TriNode value l m r) = (TriNode (f value) (inOrderMap f l) (inOrderMap f m) (inOrderMap f r))

-- 7. preOrderFold - takes a function, an accumulator value, and a trinary tree, and does a pre-order walk over the tree, applying the function on each call and returning the final result
preOrderFold :: (b -> a -> b) -> b -> TriTree a -> b
preOrderFold _ acc EmptyNode = acc
preOrderFold f acc (TriNode value l m r) = f acc' value
    where
        acc' = preOrderFold f acc'' l
        acc'' = preOrderFold f acc''' m
        acc''' = preOrderFold f acc r

instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False