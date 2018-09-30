module A2b where

removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept _ [] = []
removeAllExcept element list
    | head list == element = element : removeAllExcept element (tail list)
    | otherwise = removeAllExcept element (tail list)

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll element list
    | head list /= element = head list : removeAll element (tail list)
    | otherwise = removeAll element (tail list)

substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y list
    | head list == x = y : substitute x y (tail list)
    | otherwise = (head list) : substitute x y (tail list)

mergeTwo :: Ord a => [a] -> [a] -> [a]
mergeTwo x [] = x
mergeTwo [] y = y
mergeTwo (x:xs) (y:ys)
    | x < y = x:(mergeTwo xs (y:ys))
    | otherwise = y:(mergeTwo (x:xs) ys)

mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 (x:xs) (y:ys) (z:zs) = mergeTwo (mergeTwo (x:xs) (y:ys)) (z:zs)

-- TriTree
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)
    deriving Show

-- Test tree: TriNode 1 (TriNode 2 EmptyNode EmptyNode EmptyNode) (TriNode 3 EmptyNode EmptyNode EmptyNode) (TriNode 4 EmptyNode EmptyNode EmptyNode)

-- toList helper function
toList :: TriTree a -> [a]
toList EmptyNode = []
toList (TriNode a l m r) = [a] ++ toList l ++ toList m ++ toList r

nodeValue :: TriTree a -> a
nodeValue (TriNode a l m r) = a

-- leftChild :: TriTree a -> a
-- leftChild (TriNode a l m r) = l
-- leftChild (TriNode a EmptyNode m r) = EmptyNode
-- leftChild (TriNode a (TriNode b l' m' r') m r) = b

inTree :: Eq a => a -> TriTree a -> Bool
inTree _ EmptyNode = False
inTree x (TriNode a l m r)
    | x == a = True
    | otherwise = (inTree x l) || (inTree x m) || (inTree x r)

-- leafList :: TriTree a -> [a]
-- leafList (TriNode value EmptyNode EmptyNode EmptyNode) = value
-- leafList (TriNode value l m r) = (leafList l) : (leafList m) : (leafList r) : []

instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False