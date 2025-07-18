module Lib (mapWithIndex) where


mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = helper xs 0
    where
        helper [] _ = []
        helper (y:ys) i = f i y : helper ys (i+1)