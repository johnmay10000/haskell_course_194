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


isLocalMaxima :: Integer -> Integer -> Integer -> Bool
isLocalMaxima a b c = (a < b)  &&  (b > c)

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:[]) = []
-- localMaxima (x:y:z:[]) = if isLocalMaxima x y z then [y] else []
localMaxima (x:y:z:[]) = [y | isLocalMaxima x y z]
localMaxima (x:y:z:xs) = if isLocalMaxima x y z then y:localMaxima (y:z:xs) else localMaxima (y:z:xs)
