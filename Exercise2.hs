import           Prelude hiding (length)

-- Ex 9.2
length :: [a] -> Int
length xs = (sum .map (\x -> 1)) xs

-- Ex 9.6
squareList :: [Int] -> [Int]
sumSquareList :: [Int] -> Int
positiveList :: [Int] -> Bool

squareList ns = map (\x -> x^2) ns
sumSquareList ns = sum (squareList ns)
positiveList ns
 | ns == [] = True
 | head ns > 0 = (positiveList . tail) ns
 | otherwise = False

