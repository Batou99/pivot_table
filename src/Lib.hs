module Lib where

import Data.List
import Text.PrettyPrint.Boxes

type Header = String
data Dimension = Dimension [Header] [DisruptionPredicate]


data Disruption = Disruption { date :: String, locationType :: String, ratingLevel :: String } deriving (Show)
type DisruptionPredicate = Disruption -> Bool
type Reducer = [Disruption] -> Int
type AggregateFunction = [Disruption] -> [Int]
data PivotTable = PivotTable { headers :: [Header], rows :: [[String]] }


-- DIMENSIONS

byAny :: [DisruptionPredicate]
byAny = [const True]


byAnyOf :: [Header] -> [DisruptionPredicate]
byAnyOf headers = [\d -> locationType d `elem` headers || ratingLevel d `elem` headers]


spainLocations :: [Header]
spainLocations = [
  "Madrid",
  "Barcelona",
  "Valencia",
  "Zaragoza",
  "La coruña"
  ]


byFilter :: [Header] -> [DisruptionPredicate]
byFilter headers =
  [\d -> x `elem` [locationType d, ratingLevel d] | x <- headers]


dimension :: [Header] -> Dimension
dimension headers =
  Dimension headers $ byFilter headers


dimensionWithTotals :: [Header] -> Dimension
dimensionWithTotals headers =
  Dimension (headers ++ ["Total"]) (byFilter headers ++ byAnyOf headers)


spainDimension :: Dimension
spainDimension = dimension spainLocations


europeDimension :: Dimension
europeDimension = dimension europeLocations


europeLocations :: [Header]
europeLocations = [
  "Berlin",
  "Paris",
  "London"
  ]


spainWithTotalsDimension :: Dimension
spainWithTotalsDimension = dimensionWithTotals spainLocations


europeWithTotalsDimension :: Dimension
europeWithTotalsDimension = dimensionWithTotals europeLocations


ratingLevels :: [Header]
ratingLevels = [
  "Alto",
  "Medio",
  "Bajo",
  "Negativo"
  ]


ratingsDimension :: Dimension
ratingsDimension = dimension ratingLevels


ratingWithTotalsDimension :: Dimension
ratingWithTotalsDimension = dimensionWithTotals ratingLevels


disruptions :: [Header] -> [Disruption]
disruptions locationTypes = [
  Disruption "2001/1/1"
             (locationTypes !! mod tp locationTypesSize)
             (ratingLevels !! mod il ratingLevelsSize) | tp <- [0..], il <- [0..tp] 
  ]
  where
    ratingLevelsSize = length ratingLevels
    locationTypesSize = length locationTypes
  

-- REDUCERS
count :: Reducer
count x = fromIntegral(length x) :: Int


-- PIVOTS
pivotableList :: [DisruptionPredicate] -> [DisruptionPredicate] -> Reducer -> [Disruption] -> [Int]
pivotableList xPredicates yPredicates reducer disruptions =
  [reducer $ filter (\x -> xp x && yp x) disruptions | yp <- yPredicates, xp <- xPredicates  ]


-- VIEWS
getRow :: Int -> Int -> [a] -> [a]
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
pivotTable (Dimension xHeaders _) (Dimension yHeaders _) aggregateFunction disruptions =
  PivotTable xHeaders rowLines
  where
    list = aggregateFunction disruptions
    dimX = length xHeaders
    dimY = length yHeaders
    values = reshape dimX dimY list
    stringValues = map (map show) values
    rowLines = zipWith (:) yHeaders stringValues


generalPivotTable :: Reducer -> Dimension -> Dimension -> [Disruption] -> PivotTable
generalPivotTable reducer xDimension yDimension =
  pivotTable xDimension yDimension $ pivotableList xPredicates yPredicates reducer
  where
    Dimension _ xPredicates = xDimension
    Dimension _ yPredicates = yDimension


countPT :: Dimension -> Dimension -> [Disruption] -> PivotTable
countPT = generalPivotTable count


spainByRatingLevelPT :: [Disruption] -> PivotTable
spainByRatingLevelPT = countPT ratingsDimension spainDimension


spainWithTotalsRatingLevelPT :: [Disruption] -> PivotTable
spainWithTotalsRatingLevelPT = countPT ratingsDimension spainWithTotalsDimension


spainWithTotalsRatingLevelWithTotalsPT :: [Disruption] -> PivotTable
spainWithTotalsRatingLevelWithTotalsPT =
  countPT ratingWithTotalsDimension spainWithTotalsDimension


instance Show PivotTable where
  show (PivotTable headers rows) =
    render $ hsep 2 left (map (vcat left . map text) allData)
    where
      paddedHeaders = "**" : headers
      allData = transpose (paddedHeaders : rows)
