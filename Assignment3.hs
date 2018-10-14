import Data.List
import Data.Char

-- 1. onlyLowerCase - takes a list of strings and returns a list of strings containing only the strings that start with a lowercase letter
onlyLowercase :: [String] -> [String]
onlyLowercase = filter (all isLower)
-- onlyLowercase xs = filter isLower xs

-- 2. longestString - takes a string list and returns the longest string in the list. If there is a tie in string length, returns the first string in the tie
longestString :: [String] -> String
longestString [] = ""
longestString xs = foldl (\acc x -> if length acc >= length x then acc else x) "" xs

-- 3. longestString' - takes a string list and returns the longest string in the list. If there is a tie in string length, returns the last string in the tie
longestString' :: [String] -> String
longestString' [] = ""
longestString' xs = foldl (\acc x -> if length acc > length x then acc else x) "" xs

-- 4. longestStringHelper - generalized version of longestString, takes in a function that takes two ints and returns a bool, and a list of strings, and returns a string depending on the function passed in
longestStringHelper :: (Int -> Int -> Bool) -> [String] -> String
longestStringHelper _ [] = ""
longestStringHelper f xs = foldl (\acc x -> if f (length acc) (length x) then acc else x) "" xs

-- 4. longestString3 - works exactly like longestString, but using longestStringHelper for calculation
longestString3 :: [String] -> String
longestString3 [] = ""
longestString3 xs = longestStringHelper (\a b -> a >= b) xs

-- 4. longestString4 - works exactly like longestString', but using longestStringHelper for calculation
longestString4 :: [String] -> String
longestString4 [] = ""
longestString4 xs = longestStringHelper (\a b -> a > b) xs

-- 5. longestLowerCase - takes a list of strings and returns the longest lowercase string in the list
longestLowercase :: [String] -> String
longestLowercase [] = ""
longestLowercase xs = (longestString . onlyLowercase) xs

-- 6. revStringRev - takes a string and returns a string that contains the characters, reversed, and all lower case
revStringRev :: String -> String
revStringRev str = (reverse . map toLower) str

-- 7. firstAnswer - takes a function and a list, and applies the function to every element of the list until it either returns Just v or Nothing
firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b
firstAnswer _ [] = Nothing
firstAnswer f (x:xs) = 
    case f x of
        Nothing -> firstAnswer f xs
        Just a -> Just a

-- 8. allAnswers - takes a function and a list, applies the function to every element of the list, and returns either Nothing or a list containing the results of these calls
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers f [] = Just []
allAnswers f lst = allAnswersHelper f [] lst

-- Helper function for allAnswers
allAnswersHelper f acc [] = Just []
allAnswersHelper f acc (x:xs) = 
    case (f x) of 
        Just l -> allAnswersHelper f (acc ++ l) xs
        Nothing -> Nothing

data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)

-- 1. g takes in two functions and a Pattern. The f1 is called when a WildcardPat is found. f2 takes a string argument and is called when a VariablePat is found. The result of the g function depends on what the two functions return, but we used them for things like finding all of a certain pattern
-- g :: Num a => (() -> a) -> ([Char] -> a) -> Pattern -> a
g f1 f2 pat =
    let
      r = g f1 f2
    in
      case pat of
        WildcardPat -> f1 ()
        VariablePat x -> f2 x
        ConstructorPat (_, p) -> r p
        TuplePat values -> foldl (\i p -> (r p) + i) 0 values
        _ -> 0

-- 2. countWildCards - take a Pattern and counts the number of WildcardPat it contains
countWildCards :: Pattern -> Int
countWildCards pat =
    let
        count = 0
    in
        g (\() -> count + 1) (\_ -> 0) pat

-- 3. countWildAndVariableLengths - takes a Pattern and returns the number of Wildcard patterns plus the sum of the string lengths of the variable patterns
countWildAndVariableLengths :: Pattern -> Int
countWildAndVariableLengths pat =
    let
        count = 0
    in
        g (\() -> count + 1) (\str -> count + (length str)) pat

-- 4. countAVar - takes a String, Pattern pair, and returns the number of times the string appears in the pattern
countAVar :: (String, Pattern) -> Int
countAVar (s, pat) = 
    let
        count = 0
    in
        g (\() -> count) (\str -> if str == s then count + 1 else count) pat

-- 2. checkPat - takes a pattern and returns true if all the variables in the pattern are distinct from each other
checkPat :: Pattern -> Bool
checkPat pat = (checkRepeats . patVarNames) pat

-- Helper for checkPat, takes a Pattern and turns it into a list of the Strings it uses for variables
patVarNames :: Pattern -> [String]
patVarNames pat =
    let
        vars = []
    in
        case pat of
            VariablePat v -> v:vars
            TuplePat v -> foldl (\i p -> (patVarNames p) ++ i) vars v
            _ -> []

-- Helper for checkPat, checks whether a list contains repeats
checkRepeats :: [String] -> Bool
checkRepeats [] = True
checkRepeats (x:xs) = 
    if elem x xs
        then False
        else checkRepeats xs

-- 3. match - takes a Value, Pattern tuple and returns a list of bindings for each pattern type
match :: (Value, Pattern) -> Maybe[(String, Value)]
match (v, p) = 
    case p of
        WildcardPat -> Just []
        VariablePat s -> Just [(s, v)]
        UnitPat -> Just []
        ConstantPat a -> Just []
        ConstructorPat (s1, p) -> match (v, p)
        TuplePat ps -> match (v, head ps)

-- 4. firstMatch - takes a Value and a list of Patterns and returns the list of bindings for the first match for the value in the list of Patterns
firstMatch :: Value -> [Pattern] -> Maybe[(String, Value)]
firstMatch v p = 
    firstAnswer (\pat -> match (v, pat)) p