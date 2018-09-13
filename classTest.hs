-- 9/12

myMinimum a b = if a < b then a else b
myMinimumTwo (a,b) = if a < b then a else b

myAdd (a,b,c) (d,e,f) = (a+d, b+e, c+f)

addList xs = 
    if (null xs)
        then 0
        else (head xs) + addList (tail xs)

doubleList xs =
    if (null xs)
        then []
        else 2 * (head xs) : doubleList (tail xs) -- : adds to beginning of list
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