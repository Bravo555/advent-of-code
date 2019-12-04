import Data.Char
import qualified Data.Map as Map

windows :: Int -> [a] -> [[a]]
windows n xs = drop n $ scanl (\xs' x -> tail xs' ++ [x]) (take n xs) xs

solution = length $ filter (validPassword . windows 2 . show) [123257..647015]
    where validPassword elem = digitsDescending elem && adjDigitsMatchOnce elem
          digitsDescending = all (\(x1:x2:[]) -> digitToInt x2 >= digitToInt x1)
          adjDigitsMatchOnce elem = (length $ Map.toList $ Map.filter (== 1) (adjPairs elem)) >= 1
            where adjPairs = foldr (\(x:y:[]) pairs -> if x == y then Map.insertWith (+) x 1 pairs else pairs) Map.empty
