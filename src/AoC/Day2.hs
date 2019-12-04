{-# LANGUAGE OverloadedStrings #-}

module AoC.Day2 where

import           Control.Monad                (guard)
import           Control.Arrow                ((&&&))
import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict as I
import           Data.Text                    (pack, split, unpack)

type Input = IntMap Int
type Output = Int

fstStar :: Input -> Output
fstStar = go 0
 where
   go i v =
     let op = v I.! i
         a  = v I.! (v I.! (i+1))
         b  = v I.! (v I.! (i+2))
         s  = v I.! (i+3)
     in
     if op == 99
        then v I.! 0
        else go (i+4) (I.insert s (ex op a b) v)
   ex :: Int -> Int -> Int -> Int
   ex 1 = (+)
   ex 2 = (*)
   ex x = error $ "Invalid op code: " ++ show x


sndStar :: Input -> Output
sndStar inp =
  let ((noun, verb):_) = do
        x <- [0..99]
        y <- [0..99]
        let val = fstStar . calibrate (x, y) $ inp
        guard $ val == 19690720
        pure (x, y)
  in 100 * noun + verb


calibrate :: (Int, Int) -> Input -> Input
calibrate (i, j) = I.insert 2 j . I.insert 1 i

parse :: String -> Input
parse = I.fromList 
      . zip [0..]
      . fmap (read . unpack)
      . split (== ',')
      . pack

main :: IO  ()
main = readFile "src/input/day2"
       >>= print . (fstStar . calibrate (12, 2) &&& sndStar) . parse
