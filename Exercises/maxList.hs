{-
	maxList x is to be a recursive function that returns the largest value in x.
-}

maxList :: Ord a => [a] -> a
maxList [] = error "List is empty."
maxList (x:[]) = x
maxList (x:xs) = max x (maxList xs)

test :: Bool
test = maxList [1] == 1 &&
       maxList [1, 2, 5, 4, 2, 4] == 5 &&
       maxList [1, 2, 5, 4, 2, 4, 10] == 10

{-
	Recreate maxList as maxListFold using foldl.
	This function should not recursively call itself.
-}

maxListFold :: Ord a => [a] -> a
maxListFold [] = error "List is empty."
maxListFold (x:xs) = foldl (max) x xs

testFold :: Bool
testFold = maxListF [1] == 1 &&
           maxListF [1, 2, 5, 4, 2, 4] == 5 &&
           maxListF [1, 2, 5, 4, 2, 4, 10] == 10