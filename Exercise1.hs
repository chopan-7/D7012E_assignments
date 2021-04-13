-- Ex 3.7
threeDifferent1 :: Int -> Int -> Int -> Bool
threeDifferent1 m n p = (if m /= n && m /= n && n /= p then True else False)

threeDifferent2 :: Int -> Int -> Int -> Bool
threeDifferent2 m n p
 | m /= n && m /= p && n /= p = True
 | otherwise = False


-- Ex 3.8
twoEqual :: Int -> Int -> Bool
twoEqual n p
 | n == p = True
 | otherwise = False

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = twoEqual m n && twoEqual n p

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p x = threeEqual m n p && twoEqual p x


-- Ex 3.15
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
 | b^2 > 4.0*a*c = 2
 | b^2 == 4.0*a*c = 1
 | otherwise = 0


-- Ex 3.16
numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
 | a /= 0.0 = numberNDroots a b c
 | b /= 0.0 = 1
 | b /= 0.0 && c /= 0.0 = 0
 | otherwise = 3

-- Ex 3.17
smallerRoot , lagerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
 | nRoots == 3 || nRoots == 0 = 0.0
 | root1 < root2 = root1
 | otherwise = root2
 where
  root1 = (-b + sqrt(b^2-4*a*c))/(2*a)
  root2 = (-b - sqrt(b^2-4*a*c))/(2*a)
  nRoots = numberRoots a b c

lagerRoot a b c
 | nRoots == 3 || nRoots == 0 = 0.0
 | root1 > root2 = root1
 | otherwise = root2
 where
  root1 = (-b + sqrt(b^2-4*a*c))/(2*a)
  root2 = (-b - sqrt(b^2-4*a*c))/(2*a)
  nRoots = numberRoots a b c


