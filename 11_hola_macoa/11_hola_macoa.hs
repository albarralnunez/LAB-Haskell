main :: IO()
main = do
    inpStr <- getLine
    if (last inpStr == 'a' || last inpStr == 'A') then putStrLn "Hola maca!"
    else putStrLn "Hola maco!"