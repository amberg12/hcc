module HCC.Util
    ( map2
    , map3
    ) where

map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (x, y) = (f x, f y)

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (x, y, z) = (f x, f y, f z)