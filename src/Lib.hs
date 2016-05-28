module Lib where

import Data.List

type Headers = [String]
data Disruption = Disruption { date :: String, threatType :: String, impactLevel :: String } deriving (Show)
type DisruptionPredicate = Disruption -> Bool
type Reducer = [Disruption] -> Int
type AggregateFunction = [Disruption] -> [Int]
data PivotTable = PivotTable { headers :: [String], rows :: [[String]] }


-- DATA
threatTypes :: Headers
threatTypes = [
   "CSA/CSE",
   "Cyber crime",
   "Drugs",
   "Economic Crime",
   "Firearms"
   ]

threatTypesWithTotals :: Headers
threatTypesWithTotals = threatTypes ++ ["Total"]


impactLevels :: Headers
impactLevels = [
  "Mayor",
  "Moderate",
  "Minor",
  "Negative"
  ]


impactLevelsWithTotals :: Headers
impactLevelsWithTotals = impactLevels ++ ["Total"]


disruptions :: [Disruption]
disruptions = [
  Disruption "2001/1/1"
             (threatTypes !! mod tp threatTypesSize)
             (impactLevels !! mod il impactLevelsSize) | tp <- [0..], il <- [0..tp] 
  ]
  where
    impactLevelsSize = length impactLevels
    threatTypesSize = length threatTypes

  

-- PREDICATES
byThreatType :: [DisruptionPredicate]
byThreatType =
  [\d -> threatType d == x | x <- threatTypes]


byImpactLevel :: [DisruptionPredicate]
byImpactLevel =
  [\d -> impactLevel d == x | x <- impactLevels]


byAny :: [DisruptionPredicate]
byAny = [const True]


-- REDUCERS
count :: Reducer
count x = fromIntegral(length x) :: Int


-- PIVOTS
pivotableList :: [DisruptionPredicate] -> [DisruptionPredicate] -> Reducer -> [Disruption] -> [Int]
pivotableList xPredicates yPredicates reducer disruptions =
  [reducer $ filter (\x -> xp x && yp x) disruptions | xp <- xPredicates, yp <- yPredicates ]


threatTypeImpactLevelList :: AggregateFunction
threatTypeImpactLevelList = pivotableList byThreatType byImpactLevel count

threatTypeWithTotalsImpactLevelList :: AggregateFunction
threatTypeWithTotalsImpactLevelList = pivotableList (byThreatType ++ byAny) byImpactLevel count

threatTypeImpactLevelWithTotalsList = pivotableList byThreatType (byImpactLevel ++ byAny) count


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


pivotTable :: [String] -> [String] -> AggregateFunction -> [Disruption] -> PivotTable
pivotTable xHeaders yHeaders aggregateFunction disruptions =
  PivotTable xHeaders rowLines
  where
    list = aggregateFunction disruptions
    dimX = length xHeaders
    dimY = length yHeaders
    values = reshape dimX dimY list
    stringValues = map (map show) values
    rowLines = zipWith (:) yHeaders stringValues


threatTypeImpactLevelPT :: [Disruption] -> PivotTable
threatTypeImpactLevelPT = pivotTable impactLevels threatTypes threatTypeImpactLevelList


threatTypeWithTotalsImpactLevelPT :: [Disruption] -> PivotTable
threatTypeWithTotalsImpactLevelPT = pivotTable impactLevels threatTypesWithTotals threatTypeWithTotalsImpactLevelList

threatTypeImpactLevelWithTotalsPT = pivotTable impactLevelsWithTotals threatTypes threatTypeImpactLevelWithTotalsList


instance Show PivotTable where
  show (PivotTable headers rows) =
    headersString ++ "\n" ++ rowsString
    where
      headersString = show $ "**" : headers
      rowsString = intercalate "\n" $ map show rows
