module Main where

import Lib as L

main :: IO ()
main = do
  let disruptions = take 12 $ L.disruptions spainLocations
  print $ spainByRatingLevelPT disruptions
  print $ spainWithTotalsRatingLevelPT disruptions
  print $ spainWithTotalsRatingLevelWithTotalsPT disruptions
