module Main where

import Lib as L

main :: IO ()
main = do
  let disruptions = (take 12 $ L.disruptions L.spainDimension) ++ (take 5 $ L.disruptions L.europeDimension)
  print $ spainByRatingLevelPT disruptions
  print $ spainWithTotalsRatingLevelPT disruptions
  print $ spainWithTotalsRatingLevelWithTotalsPT disruptions
  print $ worldWithTotalsRatingLevelWithTotalsPT disruptions
