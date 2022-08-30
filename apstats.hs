module Main where

import APStats.General

-- This is a wrapper for the functions used in the APStats library

small_list :: [Int]
small_list = [3, 3, 2, 1, 1, 1, 77]

main :: IO ()
main = print (small_list)
