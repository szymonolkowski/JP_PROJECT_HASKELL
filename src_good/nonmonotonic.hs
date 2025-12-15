isNonMonotonic :: Int -> Bool
isNonMonotonic x
    | x <= 100 = False
    | otherwise =
        let digits = toDigits x
        in check digits False False
    where
        toDigits :: Int -> [Int]
        toDigits 0 = []
        toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

        check :: [Int] -> Bool -> Bool -> Bool
        check (a:b:rest) inc dec =
            let inc' = inc || b > a
                dec' = dec || b < a
            in ((inc' && dec') || check (b:rest) inc' dec')
        check _ _ _ = False

loop :: Double -> Int -> Int -> IO ()
loop p n countNonMonotonic = do
    let isNM = isNonMonotonic n
    let counter = if isNM then countNonMonotonic + 1 else countNonMonotonic

    let currentP = fromIntegral counter / (fromIntegral n + 1)

    if currentP >= p {- Odsetek ma być mniejszy ale wtedy bez sensu jest dla n = 0 ??? -}
        then do
            putStrLn $ "n = " ++ show n
            putStrLn $ "Odsetek p = " ++ show currentP
            putStrLn $ "L. niemotonicznych = " ++ show counter
        else
            loop p (n + 1) counter

main :: IO ()
main = do
    let p = 0.9
    loop p 0 0