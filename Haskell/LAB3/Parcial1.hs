


eval1 :: String -> Int
eval1 w = eval1 (words l)

eval1 :: [String] -> Int
eval1 [] = 0;
eval1 (x:xs) =  