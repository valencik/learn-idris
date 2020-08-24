import Data.List
import Data.Buffer
import System.File

labelFrom : Integer -> List a -> List (Integer, a)
labelFrom lbl [] = []
labelFrom lbl (x :: xs) = (lbl, x) :: labelFrom (lbl + 1) xs

zipWithIndex : List a -> List (Integer, a)
zipWithIndex = labelFrom 0

generate : Nat -> Nat -> List Double
generate freq n = let angleFreq = (2 * pi * cast freq) / cast n in
                      map (\x => sin (cast x * angleFreq)) (rangeFromTo 0 n)

main : IO ()
main
    = do putStrLn "Generating sine wave..."
         let nsamps = the Nat (2 * 44100)
         Just buf <- newBuffer (8 * (cast nsamps + 1))
         | Nothing => putStrLn "Buffer creation failed"
         let iSamples = zipWithIndex (generate 440 nsamps)
         _ <- traverse (\i_s => setDouble buf (8 * cast (fst i_s)) (snd i_s)) iSamples
         Right _ <- writeBufferToFile "sin440.bin64" buf (8 * cast nsamps)
         | Left err => putStrLn "Buffer write failed"
         freeBuffer buf
         putStrLn "Sine wane saved to sine440.bin64"
