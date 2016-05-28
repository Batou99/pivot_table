module Main where

import Lib as L

main :: IO ()
main = do
  let disruptions = take 12 L.disruptions
  print $ threatTypeImpactLevelPT disruptions
  print $ threatTypeWithTotalsImpactLevelPT disruptions
