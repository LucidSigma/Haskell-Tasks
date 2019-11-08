{-
	removeOdd s will remove odd-indexed characters from s.
-}

removeOdd :: String -> String
removeOdd "" = ""
removeOdd s = removeOdd' s 1
              where
                removeOdd' :: String -> Int -> String
                removeOdd' "" _ = ""
                removeOdd' (s:ss) n = if n `mod` 2 == 0
                                      then s : removeOdd' ss (n + 1)
									  else removeOdd' ss (n + 1)
									  
test :: Bool
test = removeOdd "abcdefgh" == "bdfh"