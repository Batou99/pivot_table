module Lib where

import Data.List

type ThreatType = String
type ImpactLevel = String
data Disruption = Disruption { date :: String, threatType :: ThreatType, impactLevel :: ImpactLevel } deriving (Show)
type DisruptionPredicate = Disruption -> Bool
type Reducer = [Disruption] -> Int
type AggregateFunction = [Disruption] -> [Int]
data PivotTable = PivotTable { headers :: [String], rows :: [[String]] }


-- DATA
threatTypes :: [ThreatType]
threatTypes = [
   "CSA/CSE",
   "Cyber crime",
   "Drugs",
   "Economic Crime",
   "Firearms"
   ]

threatTypesSize = length threatTypes
impactLevelsSize = length impactLevels

impactLevels :: [ImpactLevel]
impactLevels = [
  "Mayor",
  "Moderate",
  "Minor",
  "Negative"
  ]


disruptions :: [Disruption]
disruptions = [
  Disruption "2001/1/1"
             (threatTypes !! mod tp threatTypesSize)
             (impactLevels !! mod il impactLevelsSize) | tp <- [0..], il <- [0..tp] 
  ]
  

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

threatTypeWithTotalsImpactLevelList = pivotableList (byThreatType ++ byAny) byImpactLevel count


-- VIEWS
getRow :: Int -> Int -> [a] -> [a]
getRow columnsPerRow rowNum values =
  take columnsPerRow $ drop valuesToDrop values
  where
    valuesToDrop = (rowNum - 1) * columnsPerRow


reshape :: Int -> Int -> [a] -> Either String [[a]]
reshape xSize ySize values =
  if xSize * ySize /= length values
     then Left "Dimensions do not match with data"
     else Right reshaped_data
  where 
        reshaped_data = [ getRow xSize row values | row <- [1..ySize] ]


pivotTable :: Show a => [String] -> [String] -> [[a]] -> PivotTable
pivotTable xHeaders yHeaders values =
  PivotTable xHeaders rowLines
  where
    stringValues = map (map show) values
    rowLines = zipWith (:) yHeaders stringValues


threatTypeImpactLevelPT :: [Disruption] -> PivotTable
threatTypeImpactLevelPT disruptions =
  pivotTable impactLevels threatTypes values
  where
    list = threatTypeImpactLevelList disruptions
    dimX = length impactLevels
    dimY = length threatTypes
    Right values = reshape dimX dimY list


instance Show PivotTable where
  show (PivotTable headers rows) =
    headersString ++ "\n" ++ rowsString
    where
      headersString = show $ "**" : headers
      rowsString = intercalate "\n" $ map show rows
