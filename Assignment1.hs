module A1c where

-- sDotProduct takes the dot product of two tuples, multiplied by another scalar
-- a is the scalar, (b,c) is the first tuple, and (d,e) is the second tuple for the dot product
sDotProduct a (b,c) (d,e) = 
    a * (b*d + c*e)

-- distance finds the distance between two points in 2D space
-- (x1,y1) is the first point, (x2,y2) is the second point
distance (x1,y1) (x2,y2) = 
    sqrt((x2-x1)^2 + (y2-y1)^2)

-- tripleDistance finds the distance between two points in 3D space
-- (x1,y1,z1) is the first point, and (x2,y2,z2) is the second point
tripleDistance (x1,y1,z1) (x2,y2,z2) = 
    sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)

-- findMin finds the minimum value within a list of numbers
-- xs is the list to search
findMin xs = 
    if (length xs <= 1) then head xs
    else if (head xs > head(tail xs)) then findMin(tail xs)
    else findMin(head xs : tail(tail xs))

-- tupleDotProduct finds the dot product between two arbitrarily large lists of the same length
-- list1 is the first list, and list2 is the second list
tupleDotProduct list1 list2 = 
    if (length list1 <= 1) then (head list1 * head list2)
    else (head list1 * head list2) + tupleDotProduct (tail list1) (tail list2)

-- revZip2Lists takes two lists and creates a list with a pair from each list consecutively, reversed
-- list1 is the first list, list2 is the second list
revZip2Lists list1 list2 = 
    if (null list1) then []
    else (last list2, last list1) : revZip2Lists (init list1) (init list2)

-- everyThird takes a list and constructs a new list with every third element in it
-- list is the list to parse from
everyThird list = 
    if (null list) then []
    else if ((head list) `mod` 3  == 0) then (head list) : everyThird(tail list)
    else everyThird(tail list)