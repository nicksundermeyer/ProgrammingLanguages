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