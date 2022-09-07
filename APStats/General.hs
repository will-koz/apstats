module APStats.General where

import Data.List

appearencesTable :: Ord a => [a] -> [(a, Int)]
appearencesTable = ((map (\full@(first:_) -> (first, length full))) . group) . sort

appearencesTable_Ordering :: Ord b => (a, b) -> (a, b) -> Ordering
appearencesTable_Ordering (_, a) (_, b)
    | a < b = GT
    | a > b = LT
    | otherwise = EQ

mean :: (Fractional a, Foldable t) => t a -> a
mean x = (sum x) / (fromIntegral (length x))

mean_list :: [[a]] -> Int
mean_list x = mean (map length x)

median :: (Ord a, Fractional a) => [a] -> a
median xs
    | null xs = 0
    | odd len = sortedList !! mid
    | otherwise = evenMedian
    where
        evenMedian = ((sortedList !! mid) + (sortedList !! (mid - 1))) / 2
        len = length xs
        mid = len `div` 2
        sortedList = sort xs

mode :: Ord a => [a] -> a
mode x = fst $ head  $ sortBy appearencesTable_Ordering (appearencesTable x)

-- ntile :: Int -> [a] -> Maybe [a]
-- ntile x ys = take x sys
--     where
--         sys = sort ys
--
-- percentile :: [a] -> Maybe [a]
-- percentile = ntile 100 -- I don't expect this to be used very often
--
-- quartile :: [a] -> Maybe [a]
-- quartile = ntile 4
