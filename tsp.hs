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
      in loop graph (boolFalse i v) currPos n count cost ans2 (i + 1)
    | otherwise = loop graph v currPos n count cost ans (i + 1)
{-
loop graph v currPos n count cost ans i =
   if i==n
      then ans
      else if not (v !! i) && (((graph !! currPos) !! i) > 0)
         then let ans2 = tsp graph (boolTrue i v) i n (count+1) (cost + ((graph !! currPos) !! i)) ans in 
            loop graph (boolFalse i v) currPos n count cost ans2 (i+1)
         else loop graph v currPos n count cost ans (i+1)
-}


boolTrue :: Int -> [Bool] -> [Bool]
boolTrue a b = take a b ++ [True] ++ tail (drop a b)


boolFalse :: Int -> [Bool] -> [Bool]
boolFalse a b = take a b ++ [False] ++ tail (drop a b)



-- Graph to solve:
graph = [[0,10,15,20],[10,0,35,25],[15,35,0,30],[20,25,30,0]]

------------------------------------------------
v = [True] ++ replicate ((length graph)-1) False
currPos = 0
n = length graph
count = 1
cost = 0
ans = maxBound :: Int
------------------------------------------------


solve :: Int
solve = tsp graph v currPos n count cost ans
