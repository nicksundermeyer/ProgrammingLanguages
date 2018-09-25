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

mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 [] [] [] = []