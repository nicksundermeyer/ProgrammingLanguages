-- 9/17

-- Maybe example
import Data.Maybe

-- helper function to check if x has a value, or is nothing
valOf x = case x of
    (Just value) -> value
    (Nothing) -> error "No value is available"

findmax lst = 
    if null lst
        then Nothing
        else 
            let result = findmax (tail lst) -- recursively call findmax
            in
                if isJust result && ((valOf result) > head lst) -- check if result of findmax is > current head of list, and also check whether it is Nothing with isJust
                    then result
                    else (Just (head lst))

-- let example
countup n = 
    -- using let to bind temporary local function
    let count start end = 
            if start == end 
            then end : []
            else start : count (start + 1) end
    in
        count 1 n

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