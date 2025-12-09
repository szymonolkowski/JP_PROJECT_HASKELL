import Control.Monad (when)

-- Check if an integer has digits that are neither monotone increasing nor monotone decreasing
isNonMonotonic :: Int -> Bool
isNonMonotonic x = go digits False False
  where
    digits = map (read . (:[])) (show x)

    go :: [Int] -> Bool -> Bool -> Bool
    go (a:b:rest) inc dec =
        let inc' = inc || b > a
            dec' = dec || b < a
        in if inc' && dec' then True
           else go (b:rest) inc' dec'
    go _ _ _ = False

loop :: Double -> Int -> Int -> IO ()
loop p n nNonMono = do
    let pTest = fromIntegral nNonMono / fromIntegral (n + 1)
    if pTest > p
        then do
            print n
            print pTest
            print nNonMono
        else do
            let n' = n + 1
                nNonMono' = if isNonMonotonic n' then nNonMono + 1 else nNonMono
            loop p n' nNonMono'

main :: IO ()
main = do
    let p = 0.10
    let n = 100
    let nNonMono = 0
    loop p n nNonMono