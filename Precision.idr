import Data.List
import Data.Strings

toInt : Double -> Int
toInt d = cast d

rightPrecision : Int -> Double -> Int
rightPrecision n d = let base = toInt (pow 10 (cast n))
                         scaled = toInt (d * (cast base)) in
                         abs (mod scaled base)

toPrecision : Int -> Double -> String
toPrecision n d = let left = (toInt d)
                      right = rightPrecision n d in
                      show left ++ "." ++ show right

format : List Double -> String
format ns = unlines (map (toPrecision 8) ns)

main : IO ()
main
    = do putStrLn (format [0.123456789012, 2.4680123456789])
