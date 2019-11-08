{-
	crypt cipher plaintext encrypts plaintext using cipher.
	The plain text includes the 26 lowercase letters as well as '.' and ' '.
	The cipher has to be a string of 28 printable characters that indicate how the plaintext is to be crypted.
	The characters in cypher correspond to the new values for a-z, then ' ' and '.'.
	For example: A cipher of "ZqXwCeVrBtyUaszxImOKlFfdPcQv" will be the new respective characters for "abcdefghijklmnopqrstuvwxyz .".
	Every character in the cipher must be distinct (no repeated entries).
-}

import Data.Char

type LookupTable = [(Char, Char)]

crypt :: String -> String -> Either String Int
crypt _ [] = Left ""
crypt cipher plaintext = if getErrorCode cipher plaintext /= 0
                         then Right (getErrorCode cipher plaintext)
                         else Left (cipherText cipher plaintext (createLookupTable cipher validLetters))
                         where
                           cipherText :: String -> String -> LookupTable -> String
                           cipherText cipher (p:[]) table = [fromJust $ lookup p table]
                           cipherText cipher (p:ps) table = (fromJust $ lookup p table) : cipherText cipher ps table

                           createLookupTable :: String -> String -> LookupTable
                           createLookupTable (c:[]) (v:[]) = [(v, c)]
                           createLookupTable (c:cs) (v:vs) = (v, c) : createLookupTable cs vs

                           fromJust :: Maybe a -> a
                           fromJust Nothing = error "Nothing passed as argument to fromJust."
                           fromJust (Just x) = x

                           getErrorCode :: String -> String -> Int
                           getErrorCode cipher plaintext
                             | length cipher /= 28              = 1
                             | hasNonPrintableCharacters cipher = 2
                             | hasRepeatedCharacters cipher     = 3
                             | hasIllegalCharacters plaintext   = 4
                             | otherwise                        = 0

                           hasNonPrintableCharacters :: String -> Bool
                           hasNonPrintableCharacters (c:[]) = not (isPrint c)
                           hasNonPrintableCharacters (c:cs) = if isPrint c
                                                              then hasNonPrintableCharacters cs
                                                              else True   

                           hasRepeatedCharacters :: String -> Bool
                           hasRepeatedCharacters [] = False
                           hasRepeatedCharacters (c:cs) = (foldl (||) False (map (== c) cs)) || hasRepeatedCharacters cs

                           hasIllegalCharacters :: String -> Bool
                           hasIllegalCharacters [] = False
                           hasIllegalCharacters (p:ps) = not (foldl (||) False (map (== p) validLetters)) || hasIllegalCharacters ps

                           validLetters :: String
                           validLetters = "abcdefghijklmnopqrstuvwxyz ."

test :: Bool
test = crypt "ZqXwCeVrBtyUaszxImOKlFfdPcQv" "abcdefghijklmnopqrstuvwxyz ." == (Left "ZqXwCeVrBtyUaszxImOKlFfdPcQv") &&
       crypt "abc" "def" == (Right 1) && crypt "ZqXwCeVrBtyUaszxImOKlFfdPcQ\0" "abc" == (Right 2) &&
       crypt "ZqXwCeVrBtyUaszxImOKlFfdPcQZ" "abc" == (Right 3) && crypt "ZqXwCeVrBtyUaszxImOKlFfdPcQv" "/abc" == (Right 4)