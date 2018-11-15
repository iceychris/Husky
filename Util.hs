module Util where

-- foldt implementation taken from
--  https://wiki.haskell.org/Fold 
foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
 
foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))
 
pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t



-- convert a float with a maximum to an int
-- ?
displayable :: Float -> Float -> Int
displayable val maxi = round $ (*) val $ maxi

-- (tolerance, a, b)
roughlyEqual :: Real a => a -> a -> a -> Bool
roughlyEqual tol a b = (abs (a-b)) < tol
