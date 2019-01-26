import Data.Word
import Text.Read
import Data.Maybe
import Data.Tuple

data IPv4 = IPv4 Word8 Word8 Word8 Word8
    deriving (Show)

makeIPv4 :: [Word8] -> IPv4
makeIPv4 [w1, w2, w3, w4] = IPv4 w1 w2 w3 w4
makeIPv4 _ = undefined

maxWord8 :: Int
maxWord8 = fromIntegral (maxBound :: Word8)

safeWord8 :: String -> Maybe Word8
safeWord8 str = 
    case (readMaybe str :: Maybe Int) of
        Just n -> if n > maxWord8 then Nothing else Just (fromIntegral n)
        Nothing -> Nothing

consumeWord8 :: String -> [(String, Word8)]
consumeWord8 "" = []
consumeWord8 ('0':res) = [(res, 0)]
consumeWord8 str = 
    let
        splits = map (swap . flip splitAt str) [1..length str]
        readWord8 sp = sequence (safeWord8 <$> sp)
        possibleReads = sequence (takeWhile isJust (map readWord8 splits))
    in
        fromMaybe [] possibleReads


-- Generate all the possible ways to parse a string in to `n` Word8s
possibleWord8s :: Int -> String -> [[Word8]]
possibleWord8s 0 str 
    | null str = return []
    | otherwise = mempty
possibleWord8s n str = do
    (rest, i) <- consumeWord8 str
    restWs <- (possibleWord8s (n - 1) rest)
    return (i : restWs)


possibleAddresses :: String -> [IPv4]
possibleAddresses = map makeIPv4 . possibleWord8s 4