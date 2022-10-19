module Main where

import APStats.General

-- This is a wrapper for the functions used in the APStats library

larger_list = [1, 10, 100, 1000]
long_list = [0, 1, 1, 2, 2, 2, 3, 3, 4, 4, 6, 6, 6, 6, 7, 7, 7, 8, 8, 8, 8, 8, 9, 10, 10, 12, 12,
    13, 15, 16, 21]
pairs = [(0, 1), (2, 3)]
small_list = [3, 3, 2, 1, 1, 1, 77]
word_list = ["Hi", "my", "name", "is", "Hal"]

main :: IO ()
main = print (standard_deviation_pop long_list)
