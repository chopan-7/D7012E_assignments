-- Author: Chonratid Pangdee, chopan-7@student.ltu.se
import           Prelude hiding (and, elem, lookup, or, product)

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

-- Ex 4.7
natMul :: Int -> Int -> Int
natMul a b
 | a == 1 = b
 | otherwise = b + natMul (a-1) b

-- Ex 4.8
sqrtInt :: Int -> Int -> Int
sqrtInt n k
 | k^2 > n = (k-1)
 | otherwise = sqrtInt n (k+1)

-- Ex 4.9
f :: Int -> Int
f 0 = 0
f 1 = 44
f 2 = 17
f _ = 0

recMax :: Int -> Int
recMax n
 | n == 0 = f n
 | otherwise = max (f n) (recMax (n-1))

-- Ex 4.14
powerOfTwo :: Int -> Int
powerOfTwo n
 | n == 0 = 1
 | otherwise = 2 * (powerOfTwo (n-1))

-- Ex 5.2
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a,b,c)
 | c > b  && b > a = (a,b,c)
 | b < a = orderTriple (middleThree trip, minThree trip, maxThree trip)
 | c < b = orderTriple (minThree trip, maxThree trip, middleThree trip)
 where trip = (a,b,c)

minThree :: (a,b,c) -> a
minThree (x,_,_) = x

middleThree :: (a,b,c) -> b
middleThree (_,x,_) = x

maxThree :: (a,b,c) -> c
maxThree (_,_,x) = x

-- Ex 5.10
divisors :: Int -> [Int]
divisors x = [i | i <- [1..x], (x `mod` i) == 0]

isPrime :: Int -> Bool
isPrime x
 | length (divisors x) < 3 = True
 | otherwise = False


-- Ex 5.11
list511 :: [Int]
list511 = [2, 3, 5, 12, 22, 5, 3]

matches :: Int -> [Int] -> [Int]
matches i xs = [x | x <- xs, x==i]

isElem :: Int -> [Int] -> Bool
isElem k xs
 | length (matches k xs) > 0 = True
 | otherwise = False

-- Ex 5.18
-- shift :: ((Int,Int), Int) -> (Int, (Int, Int))
shift :: ((a,b),c) -> (a,(b,c))
shift ((x,y),z) = (x,(y,z))

-- Ex 5.22
onSeparateLines :: [String] -> String
onSeparateLines (x:xs)
 | length (x:xs) == 1 = x ++ "\n"
 | otherwise = x ++ "\n" ++ onSeparateLines xs

-- Ex 5.23
duplicate :: String -> Int -> String
duplicate str n
 | n <= 0 = ""
 | n == 1 = str
 | otherwise = str ++ duplicate str (n-1)

-- Ex 6.29
type Name = String
type Barcode = Int
type Price = Int
type Database = [(Barcode, Name, Price)]

codeIndex :: Database
codeIndex = [
 (4719, "Fish Fingers", 121),
 (5643, "Nappies", 1010),
 (3814, "Orange Jelly", 56 ),
 (1111, "Hula Hoops", 21),
 (1112, "Hula Hoops (Giant)",133),
 (1234, "Dry Sherry, 1lt", 540)]

type TillType = [Barcode]
type BillType = [(Name, Price)]

makeBill :: TillType -> BillType
formatBill :: BillType -> String
produceBill :: TillType -> String

produceBill = formatBill . makeBill

lineLength :: Int
lineLength = 30

formatPence :: Price -> String
formatPence p = show (p `div` 100) ++"." ++ show (p `mod` 100)

formatLine :: (Name, Price) -> String
formatLine (n, p) = n ++ (duplicate "." (lineLength - (length n) - (length (formatPence p)))) ++ formatPence p ++ "\n"

formatLines :: [(Name, Price)] -> String
formatLines (x:xs)
 | length (x:xs) == 1 = formatLine x
 | otherwise = formatLine x ++ formatLines xs

makeTotal :: BillType -> Price
makeTotal xs = sum [snd x | x <- xs]

formatTotal :: Price -> String
formatTotal total = "\nTotal" ++ (duplicate "." (lineLength - 5 - (length (formatPence total)))) ++ formatPence total ++ "\n"

formatBill xs = pushRight ++ title ++ "\n\n" ++ formatLines xs ++ formatDiscount discount ++ formatTotal total
 where title = "Haskell Stores"
       pushRight = duplicate " " ((lineLength - (length title)) `div` 2)
       discount = makeDiscount xs
       total = (makeTotal xs) - discount


look :: Database -> Barcode -> (Name, Price)
look [] _ = ("Unknown Item", 0)
look (x:xs) barcode
 | minThree x == barcode = (middleThree x, maxThree x)
 | otherwise = look xs barcode

lookup :: Barcode -> (Name, Price)
lookup barcode = look codeIndex barcode

makeBill items = [lookup item | item <- items]

itemlist :: TillType
itemlist = [1234, 4719, 3814, 1112, 1113, 1234]

makeDiscount :: BillType -> Price
makeDiscount items = ((sum [1 | item <- items, (fst item) == "Dry Sherry, 1lt"]) `div` 2) * 100

formatDiscount :: Price -> String
formatDiscount p = "\nDiscount" ++ dots ++ fDiscount ++ "\n"
 where fDiscount = formatPence p
       dots = duplicate "." (lineLength - 8 - (length fDiscount))

-- Ex 7.2 (pattern matching)
addFirstTwo :: [Int] -> Int
addFirstTwo []     = 0
addFirstTwo (x:xs) = x + (head xs)

-- Ex 7.4
product :: [Int] -> Int
product []     = 1
product (x:xs) = x * product xs


-- Ex 7.5
and, or :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs
or []     = False
or (x:xs) = x || or xs

-- Ex 7.7
unique :: [Int] -> [Int]
unique xs = [x | x <- xs, length (matches x xs) == 1]

-- Ex 7.27
isPalin :: String -> Bool
isPalin [a] = True
isPalin xs
 | head st == last st = (isPalin . init . tail) st
 | otherwise = False
 where st = [if isUpper x then fromUpper x else x | x <- xs]

formatString :: String -> String
formatString st = [if isUpper x then fromUpper x else x | x <- formSt]
 where fromSt = [x | x <- st, (fromEnum x >= 65 && fromEnum x 90) || (fromEnum x >= 97 && fromEnum x 122)]

toUpper, fromUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + offset)
fromUpper ch = toEnum (fromEnum ch - offset)

isUpper :: Char -> Bool
isUpper ch = if fromEnum ch >=65 && fromEnum ch <= 90 then True else False

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

