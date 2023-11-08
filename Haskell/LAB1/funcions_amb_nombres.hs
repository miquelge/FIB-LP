-- Funcions amb nombres


absValue :: Int -> Int
absValue x =
    if (x < 0) then (- x)
    else x 


power :: Int -> Int -> Int
power x y =
    case y of
    0 -> 1
    y -> (x * (power x (y - 1)))


isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = auxIsPrime x (x - 1)
auxIsPrime :: Int -> Int -> Bool
auxIsPrime x y =
    if (y == 1) then True
    else if (x `mod` y) == 0 then False
    else auxIsPrime x (y - 1)


slowFib :: Int -> Int
slowFib n
    | (n == 0) = 0
    | (n == 1) = 1
    | otherwise = (slowFib (n - 1)) + (slowFib (n - 2))


quickFib :: Int -> Int
quickFib n = fst (auxQuickFib n)

auxQuickFib :: Int -> (Int, Int)
auxQuickFib 0 = (0, 1)
auxQuickFib n = (nessim_1, nessim + nessim_1) 
    where (nessim, nessim_1) = auxQuickFib (n-1)

