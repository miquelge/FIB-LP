-- Hola Maco/a!

saluda :: String -> String 
saluda n = 
    if (last n == 'a' || last n == 'A') then "Hola maca!"
    else "Hola maco!"

main = do
    nom <- getLine
    let sortida = saluda nom
    putStrLn sortida