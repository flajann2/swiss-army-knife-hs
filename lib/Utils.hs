module Utils where

isExclusiveOr :: [Bool] -> Bool
isExclusiveOr bs = (length $ filter id bs) == 1

isExclusiveOrNone :: [Bool] -> Bool
isExclusiveOrNone bs = (length $ filter id bs) `elem` exorno
  where 
   exorno = [0, 1]

