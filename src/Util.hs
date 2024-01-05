module Util where

{- Composition Operators -}
-- there are libraies for this, but these are too easy to write to bother downloading them

infixr 8 .:
infixr 8 .:.

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = (.) . (.) . (.)
