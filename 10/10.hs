import           Data.Ratio
import           Data.Decimal

type Asteroid = (Int, Int)

input =
    ".#..##.###...#######\n\
\`##.############..##.\n\
\.#.######.########.#\n\
\.###.#######.####.#.\n\
\#####.##.#.##.###.##\n\
\..#####..#.#########\n\
\####################\n\
\#.####....###.#.#.##\n\
\##.#################\n\
\#####.##.###..####..\n\
\..######..##.#######\n\
\####.##.####...##..#\n\
\.#####..#.######.###\n\
\##...#.##########...\n\
\#.##########.#######\n\
\.####.#.###.###.#.##\n\
\....##.##.###..#####\n\
\.#.#.###########.###\n\
\#.#.#.#####.####.###\n\
\###.##.####.##.#..##"

mapToAsteroids :: String -> [Asteroid]
mapToAsteroids map' = map fst $ filter (\(_, c) -> c == '#') $ enumerate rows
  where
    rows = lines map'
    enumerate rows = concat
        $ map (\(i, l) -> zip (zip [0 ..] (repeat i)) l) (zip [0 ..] rows)

asteroids = mapToAsteroids input

uniqueVectors :: [(Float, Float)] -> [(Float, Float)]
uniqueVectors []              = []
uniqueVectors ((x1, y1) : vs) = (x1, y1) : (uniqueVectors deduplicated)
  where
    deduplicated = filter
        (\(x2, y2) -> abs (x2 - x1) > threshold || abs (y2 - y1) > threshold)
        vs
    threshold = 0.0000001


unit (x, y) = let len = sqrt (x ^ 2 + y ^ 2) in (x / len, y / len)

asteroidsVisibleFrom :: [Asteroid] -> Asteroid -> [(Float, Float)]
asteroidsVisibleFrom as a =
    let vec (x1, y1) (x2, y2) =
                (fromIntegral (x2 - x1), fromIntegral (y2 - y1))
    in  uniqueVectors $ map (unit . vec a) $ filter (/= a) as

asteroidVisibilityField :: [Asteroid] -> [Int]
asteroidVisibilityField asteroids = map (length . visibleFrom) asteroids
    where visibleFrom = asteroidsVisibleFrom asteroids

bestPosition = zip (asteroidVisibilityField asteroids) asteroids
