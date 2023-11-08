-- IMC


definicio :: Float -> String
definicio x =
    if (x < 18) then "magror"
    else if (x < 25) then "corpulencia normal"
    else if (x < 30) then "sobrepes"
    else if (x < 40) then "obesitat"
    else "obesitat morbida"

imc :: Float -> Float -> Float
imc m h = (m / (h^2))

index :: String -> String
index s = 
    (head (words s)) ++ ": " ++ (definicio (imc m h))
    where 
        m = read ((words s)!!1) :: Float
        h = read ((words s)!!2) :: Float

main = do
    line <- getLine
    if (line == "*") then return()
    else 
        do  putStrLn (index line)
            main
       
