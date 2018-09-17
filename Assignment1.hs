sDotProduct a (b,c) (d,e) = 
    a * (b*d + c*e)

distance (x1,y1) (x2,y2) = 
    sqrt((x2-x1)^2 + (y2-y1)^2)

tripleDistance (x1,y1,z1) (x2,y2,z2) = 
    sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)

findMin xs = 
    if (length xs <= 1) then head xs
    else if (head xs > head(tail xs)) then findMin(tail xs)
    else findMin(head xs : tail(tail xs))

tupleDotProduct list1 list2 = 
    if (length list1 <= 1) then (head list1 * head list2)
    else (head list1 * head list2) + tupleDotProduct (tail list1) (tail list2)

revZip2Lists list1 list2 = 
    if (null list1) then []
    else (last list2, last list1) : revZip2Lists (init list1) (init list2)

everyThird list = 
    if (null list) then []
    else if ((head list) `mod` 3  == 0) then (head list) : everyThird(tail list)
    else everyThird(tail list)