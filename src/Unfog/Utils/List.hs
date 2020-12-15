module Unfog.Utils.List where

zipProduct :: [a] -> [b] -> [(a, b)]
zipProduct [] _ = []
zipProduct (xa : a) b = zip (repeat xa) b ++ zipProduct a b
