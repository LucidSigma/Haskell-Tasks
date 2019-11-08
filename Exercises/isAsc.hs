{-
	isAsc x tests if the list is sorted in ascended order.
-}

isAsc :: Ord a => [a] -> Bool
isAsc [] = error "List is empty."
isAsc (x:[]) = True
isAsc (x:xs) = if x > head xs then False else isAsc xs

test :: Bool
test = isAsc [1, 2, 3] && not (isAsc [1, 2, 2, 1]) && isAsc [1, 2, 2]