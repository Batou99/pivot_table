module Main where

import Lib as L

main :: IO ()
main = do
  let ratings = (take 120 $ L.ratings L.spainDimension) ++ (take 50 $ L.ratings L.europeDimension)
  print $ spainByRatingLevelPT ratings
  print $ spainWithTotalsRatingLevelPT ratings
  print $ spainWithTotalsRatingLevelWithTotalsPT ratings
  print $ worldWithTotalsRatingLevelWithTotalsPT ratings
