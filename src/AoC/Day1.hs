module AoC.Day1 where

import Control.Arrow ((&&&))

type Input = [Int]
type Output = Int

fuel :: Int -> Int
fuel x = max 0 (div x 3 - 2)

fstStar :: Input -> Output
fstStar =  sum . map fuel

fuel' :: Int -> Int
fuel' 0 = 0
fuel' x = let v = fuel x
          in  v + fuel' v

sndStar :: Input -> Output
sndStar = sum . map fuel'

main :: IO  ()
main = do
  input <- fmap read . lines <$> readFile "src/input/day1" 
  print . (fstStar &&& sndStar) $ input
