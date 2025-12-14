findPytriples :: Int -> [(Int, Int, Int)]
findPytriples n =
    [ (a, b, c)
    | c <- reverse [1 .. n `div` 2 - 1]
    , let a_plus_b = n - c
    , let min_b = a_plus_b `div` 2
    , b <- reverse [1 .. c - 1]
    , b >= min_b
    , let a = a_plus_b - b
    , gcd a (gcd b c) == 1
    , a > 0
    , a * a + b * b == c * c
    ]

main :: IO ()
main = do
    putStrLn "Podaj sumę boków N (np. 12): "
    inputStr <- getLine
    
    case reads inputStr :: [(Int, String)] of
        [(n, "")] | n >= 12 -> do
            putStrLn $ "Szukam trójek pitagorejskich dla sumy N = " ++ show n ++ ":"
            let results = findPytriples n
            
            if null results
                then putStrLn "Nie znaleziono trójek pitagorejskich dla tej sumy."
                else mapM_ print results
            
        _ -> putStrLn "Niepoprawne wejście. Podaj poprawną liczbę całkowitą (N >= 12)."