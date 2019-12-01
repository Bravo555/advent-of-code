-- part 1 and 2

import System.IO

solution = fmap (foldr1 (+) . map (fuel . read) . lines) (readFile "input.txt")
    where
        fuel mass
            | fuelMass <= 0 = 0
            | otherwise = fuelMass + fuel fuelMass
                where fuelMass = mass `div` 3 - 2