{-
	insertAt n x xs will insert the value x into the list xs at index n.
	You can assume that n will be positive.
	If n is greater than the size of the list, then x is to be added at the end of xs.
-}

insertAt :: Int -> a -> [a] -> [a]
insertAt _ x [] = [x]
insertAt 0 x xs = x : xs
insertAt n x xs = l ++ (x : r)
                  where
                    (l, r) = splitAt n xs

test :: Bool
test = insertAt 3 '-' "abcde" == "abc-de" &&
       insertAt 2 100 [1..5] == [1, 2, 100, 3, 4, 5]