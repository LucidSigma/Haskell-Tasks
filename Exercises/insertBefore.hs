{-
	insertAtBefore pat str xs will insert str into xs at the position immediately before the pattern pat.
	If pat is not in xs, then no insertion will occur.
	Use the insertAt n x xs function which you have written before to help.
-}

insertBefore :: String -> String -> String -> String
insertBefore pat str xs = if index == Nothing
                          then xs
                          else insertAtString (fromJust index) str xs
                          where
                            index = findStr pat xs

                            fromJust :: Maybe a -> a
                            fromJust Nothing = error "Nothing passed as argument to fromJust."
                            fromJust (Just x) = x

                            -- The insertAt function only works on atom types, not lists; so each character must be inserted one at a time
                            insertAtString :: Int -> String -> String -> String
                            insertAtString _ [] s = s
                            insertAtString n (p:ps) s = insertAtString (n + 1) ps (insertAt n p s)

insertAt :: Int -> a -> [a] -> [a]
insertAt _ x [] = [x]
insertAt 0 x xs = x : xs
insertAt n x xs = l ++ (x : r)
				  where
				    (l, r) = splitAt n xs

findStr :: String -> String -> Maybe Int
findStr pat s = find' 0 (length pat) pat s
				where
				  find' :: Int -> Int -> String -> String -> Maybe Int
				  find' i _ [] _ = Just i
				  find' _ _ _ [] = Nothing
				  find' i n pat xs = if pat == take n xs
				  				     then Just i
								     else find' (i + 1) n pat (tail xs)

test :: Bool
test = insertBefore "xx" "aa" "bbbb" == "bbbb" &&
       insertBefore "xx" "aa" "bbbbxx" == "bbbbaaxx" &&
       insertBefore "" "aa" "bbbbxx" == "aabbbbxx"