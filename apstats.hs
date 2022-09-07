module Main where

import APStats.General

-- This is a wrapper for the functions used in the APStats library

larger_list = [1, 10, 100, 1000]
pairs = [(0, 1), (2, 3)]
small_list = [3, 3, 2, 1, 1, 1, 77]
wordlist = ["Hi", "my", "name", "is", "Hal"]

main :: IO ()
main = print (median small_list)
