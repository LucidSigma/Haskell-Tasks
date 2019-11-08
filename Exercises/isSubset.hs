{-
	isSubset x y should test if all the elements in list x are fully contained in list y.
	Both x and y can contain duplicates.
	For example, if x had two of the same value and y only had that value once, then x is not a subset.
-}

isSubset :: [Int] -> [Int] -> Bool
isSubset [] _ = True
isSubset _ [] = False
isSubset (x:xs) y = if isInList x y
                    then isSubset xs (removeElement x y)
                    else False
                    where
                      isInList :: Int -> [Int] -> Bool
                      isInList _ [] = False
                      isInList v (x:xs) = if v == x
                                          then True
                                          else isInList v xs

                      removeElement :: Int -> [Int] -> [Int]
                      removeElement _ [] = []
                      removeElement v (x:xs) = if v == x
                                               then xs
											   else (x : removeElement v xs)
											   
test :: Bool
test = isSubset [1, 2, 2] [3, 5, 1, 3, 2, 8, 2]