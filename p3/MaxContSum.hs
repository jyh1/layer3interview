import Data.List

maxContSum :: [Integer] -> Integer
maxContSum = maximum . maxContEnd

maxContEnd :: [Integer] -> [Integer]
maxContEnd = snd . mapAccumL step 0
    where
        step prevSum n = (max 0 cur, cur) 
            where cur = prevSum + n