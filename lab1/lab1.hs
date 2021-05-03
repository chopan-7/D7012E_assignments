-- Author: Chonratid Pangdee, chopan-7@student.ltu.se
-- Date: April, 2021

-- find sublists in pair of indices
findSubRange :: Int -> Int -> [Int] -> [(Int, Int)]
findSubRange i j list
 | i == length list = [(i, j)]
 | j > i = concat [findSubRange i (j-1) list, [(i,j)]]
 | j == i = concat [[(i,j)], findSubRange (i+1) (length list) list]

-- get all sublists given pairs of indices
allSubLists :: [(Int,Int)] -> [Int] -> [[Int]]
allSubLists range list = [getSubList r list | r <- range]

-- get sublist from list given pair of indices
getSubList :: (Int, Int) -> [Int] -> [Int]
-- getSubList range list = [list!!(index-1) | index <- [(fst range)..(snd range)]]
getSubList range list = take (snd range - fst range + 1) (drop (fst range - 1) list)


-- compare the sublist size and return the pair with minimum list sum
minSize :: (Int, Int) -> (Int, Int) -> [Int] -> (Int, Int)
minSize sub1 sub2 list
 | (sum (getSubList sub1 list)) > (sum (getSubList sub2 list)) = sub2
 | otherwise = sub1

-- recursivly compair all sublist indices and return the minumum
getMin :: (Int, Int) -> [(Int, Int)] -> [Int] -> (Int, Int)
getMin min (x:xs) list
 | length (x:xs) == 1 = minSize min x list
 | otherwise = getMin (minSize min x list) xs list

-- filter out the k smallest sublist
getKSmallest :: Int -> [(Int,Int)]  -> [Int] -> [(Int, Int)]
getKSmallest k subs list
 | k == 1 = [min]
 | otherwise = concat [[min], getKSmallest (k-1) nextSub list]
 where min = getMin (head subs) (tail subs) list
       nextSub = filter (/= min) subs

-- format the printout lines
formatPrintLines :: [(Int, Int)] -> [Int] -> IO ()
formatPrintLines indices list
 | length indices == 1 = printLine
 | otherwise =
  do printLine
     formatPrintLines (drop 1 indices) list
 where pair = head indices
       sublist = getSubList pair list
       size = sum sublist
       printLine = putStrLn ((show size) ++ "\t" ++ (show $ fst pair) ++ "\t" ++ (show $ snd pair) ++ "\t" ++ (show sublist))

-- main function
smallestKset :: Int -> [Int] -> IO ()
smallestKset k list =
 do putStrLn ("Entire list:" ++ show list)
    putStrLn ""
    putStrLn "Size \ti\tj\tsublist"
    formatPrintLines smallest list
    putStrLn ""
 where smallest = getKSmallest k (findSubRange 1 (length list) list) list

-- test cases
testList = [-1, 2, -3, 4, -5]
testCase1 = [x*(-1)^x | x <- [1..100]]
testCase2 = [24, -11, -34, 42, -24, 7, -19, 21]
testCase3 = [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3]

main =
 do putStrLn "------- Test case 1 ---------"
    smallestKset 15 testCase1
    putStrLn "------- Test case 2 ---------"
    smallestKset 6 testCase2
    putStrLn "------- Test case 3 ---------"
    smallestKset 8 testCase3
