{-
	barchart w lvpairs should create a horizontal bar chart.
	Create a helper bar w (l, v) function to create a single bar.
	One '*' character is drawn for each value, and the label is shown padded out/truncated to w characters.
	Use the map and unlines library functions to help.
-}

type LabelValuePair = (String, Int)

barchart :: Int -> [LabelValuePair] -> String
barchart w lvpairs = unlines $ map (bar w) lvpairs

bar :: Int -> LabelValuePair -> String
bar w (label, value) = setToWidth w label ++ " " ++ createBar value
                       where
						 createBar :: Int -> String
						 createBar 0 = ""
                         createBar l = '*' : createBar (l - 1)

                         setToWidth :: Int -> String -> String
                         setToWidth 0 _ = []
                         setToWidth w [] = ' ' : setToWidth (w - 1) []
                         setToWidth w (x:xs) = x : setToWidth (w - 1) xs

test :: Bool
test = barchart 5 [("xxxx", 5), ("yyy", 8), ("zzzzzzzz", 3)] == "xxxx  *****\nyyy   ********\nzzzzz ***\n"