module Utils where

unique :: [Integer] -> Integer
unique ids =
  let unique' current =
        if elem current ids then unique' (succ current) else current
  in  unique' 0
