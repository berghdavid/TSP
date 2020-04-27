{-
LTH
Evaluation of Software Systems (EDAA35)
Solution for Travelling Salesman Problem (TSP) using Haskell
David Bergh (berghdavid@hotmail.com)

Solution is based on Java implementation of same problem made by Rajput-Ji.
https://www.geeksforgeeks.org/travelling-salesman-problem-implementation-using-backtracking/
-}

module TSP where
tsp :: [[Int]] -> [Bool] -> Int -> Int -> Int -> Int -> Int -> Int
tsp graph v currPos n count cost ans = 
   if (count==n) && (head (graph !! currPos) > 0)
      then min ans (cost + head (graph !! currPos))
      else loop graph v currPos n count cost ans 0


loop :: [[Int]] -> [Bool] -> Int -> Int -> Int -> Int -> Int -> Int -> Int
loop graph v currPos n count cost ans i
    | i == n = ans
    | not (v !! i) && (((graph !! currPos) !! i) > 0)
    = let
        ans2 = tsp graph (boolTrue i v) i n (count + 1) (cost + ((graph !! currPos) !! i)) ans
      in loop graph v currPos n count cost ans2 (i + 1)
    | otherwise = loop graph v currPos n count cost ans (i + 1)


-- Sets boolean on place a in the list to True
boolTrue :: Int -> [Bool] -> [Bool]
boolTrue a list = take a list ++ [True] ++ tail (drop a list)


-- Graph to solve:
graph = [[0,10,15,20],[10,0,35,25],[15,35,0,30],[20,25,30,0]]

-- Example graph
-- graph = [[0,1,15,20],[10,0,2,25],[15,35,0,3],[4,25,30,0]]

------------------------------------------------
v = True : replicate (length graph - 1) False
currPos = 0
n = length graph
count = 1
cost = 0
ans = maxBound :: Int
------------------------------------------------


solve :: Int
solve = tsp graph v currPos n count cost ans
