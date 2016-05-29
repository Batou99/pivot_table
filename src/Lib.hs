module Lib (disruptions,
            spainDimension,
            europeDimension,
            spainByRatingLevelPT,
            spainWithTotalsDimension,
            spainWithTotalsRatingLevelPT,
            spainWithTotalsRatingLevelWithTotalsPT
            ) where

import Data.List
import Text.PrettyPrint.Boxes

--TYPES
type Dimension = [Header]
type DisruptionPredicate = Disruption -> Bool
type Reducer = [Disruption] -> Int
type AggregateFunction = [Disruption] -> [Int]

data Header = Header String | Total
data Disruption = 
  Disruption { date :: String, locationType :: String, ratingLevel :: String } deriving (Show)
data PivotTable = PivotTable { dimX :: Dimension, dimY :: Dimension, contents :: [[Int]] }

instance Show PivotTable where
  show (PivotTable dimX dimY contents) =
    render $ hsep 2 left (map (vcat left . map text) allData)
    where
      hStrings = "**" : map show dimX
      vStrings = map show dimY
      rows = zipWith (:) vStrings $ (map . map) show contents
      allData = transpose (hStrings : rows)

instance Show Header where
  show (Header s) = s
  show Total = "Total"


-- DIMENSIONS
predicate :: Header -> DisruptionPredicate
predicate (Header name) = \d -> name `elem` [locationType d, ratingLevel d]
predicate Total = const True


predicates :: Dimension -> [DisruptionPredicate]
predicates = map predicate


spainDimension :: Dimension
spainDimension = map Header [
  "Madrid",
  "Barcelona",
  "Valencia",
  "Zaragoza",
  "La coruña"
  ]


europeDimension :: Dimension
europeDimension = map Header [
  "Berlin",
  "Paris",
  "London"
  ]


spainWithTotalsDimension :: Dimension
spainWithTotalsDimension = spainDimension ++ [Total]


europeWithTotalsDimension :: Dimension
europeWithTotalsDimension = europeDimension ++ [Total]


ratingsDimension:: Dimension
ratingsDimension = map Header [
  "Alto",
  "Medio",
  "Bajo",
  "Negativo"
  ]


ratingWithTotalsDimension :: Dimension
ratingWithTotalsDimension = ratingsDimension ++ [Total]


-- GENERATORS
disruptions :: Dimension -> [Disruption]
disruptions locationsDimension = [
  disruptionGenerator 
             (locationsDimension !! mod tp locationsDimensionSize)
             (ratingsDimension !! mod il ratingsDimensionSize) | tp <- [0..], il <- [0..tp] 
  ]
  where
    ratingsDimensionSize = length ratingsDimension
    locationsDimensionSize = length locationsDimension
  

disruptionGenerator :: Header -> Header -> Disruption
disruptionGenerator (Header location) (Header rating) =
  Disruption "2001/1/1" location rating


-- PIVOTS
spainByRatingLevelPT :: [Disruption] -> PivotTable
spainByRatingLevelPT = countPT ratingsDimension spainDimension


spainWithTotalsRatingLevelPT :: [Disruption] -> PivotTable
spainWithTotalsRatingLevelPT = countPT ratingsDimension spainWithTotalsDimension


spainWithTotalsRatingLevelWithTotalsPT :: [Disruption] -> PivotTable
spainWithTotalsRatingLevelWithTotalsPT =
  countPT ratingWithTotalsDimension spainWithTotalsDimension


-- REDUCERS
count :: Reducer
count x = fromIntegral(length x) :: Int


-- PRIVATE
pivotableList :: Dimension -> Dimension -> Reducer -> [Disruption] -> [Int]
pivotableList xDimension yDimension reducer disruptions =
  [reducer $ filter (\x -> xp x && yp x) disruptions | yp <- yPredicates, xp <- xPredicates  ]
  where
    xPredicates = predicates xDimension
    yPredicates = predicates yDimension


-- VIEWS
getRow :: Int -> Int -> [Int] -> [Int]
getRow columnsPerRow rowNum values =
  take columnsPerRow $ drop valuesToDrop values
  where
    valuesToDrop = (rowNum - 1) * columnsPerRow


reshape :: Int -> Int -> [Int] -> [[Int]]
reshape xSize ySize values =
  [ getRow xSize row padded_values | row <- [1..ySize] ]
  where
    total_elements = xSize * ySize
    zeros = take total_elements [0,0..]
    padded_values = values ++ zeros


pivotTable :: Dimension -> Dimension -> AggregateFunction -> [Disruption] -> PivotTable
pivotTable xDimension yDimension aggregateFunction disruptions =
  PivotTable xDimension yDimension values
  where
    list = aggregateFunction disruptions
    dimX = length xDimension
    dimY = length yDimension
    values = reshape dimX dimY list


generalPivotTable :: Reducer -> Dimension -> Dimension -> [Disruption] -> PivotTable
generalPivotTable reducer xDimension yDimension =
  pivotTable xDimension yDimension $ pivotableList xDimension yDimension reducer


countPT :: Dimension -> Dimension -> [Disruption] -> PivotTable
countPT = generalPivotTable count
