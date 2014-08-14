module Golf where

import Control.Applicative
import Data.List


takeNth :: Int -> [a] -> [a]
takeNth _ [] = []
takeNth n xs = case drop (n-1) xs of
            [] -> []
            (y:ys) -> y : takeNth n ys

genList :: Int -> [a] -> [[a]]
genList _ [] = []
genList 0 _ = []
genList n l = takeNth (length l - n + 1) l : genList (n-1) l


skips :: [a] -> [[a]]
skips [] = []
skips l = genList (length l) l
