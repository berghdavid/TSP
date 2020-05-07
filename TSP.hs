{-
LTH
Evaluation of Software Systems (EDAA35)
Solution for Travelling Salesman Problem (TSP) using Haskell
David Bergh (berghdavid@hotmail.com)

Solution is based on Java implementation of same problem made by Rajput-Ji.
https://www.geeksforgeeks.org/travelling-salesman-problem-implementation-using-backtracking/

Compile and run to test different number of iterations and graphs.
-}

module Main where
import Data.List
import Data.Time.Clock


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


------------------------------------------------
-- Graph to solve:

graph4 = [[0,10,15,20],[10,0,35,25],[15,35,0,30],[20,25,30,0]]

graph5 = [[0,10,15,20,25],[10,0,35,25,15],[15,35,0,30,10],
  [20,25,30,0,20],[10,15,20,25,0]]

graph8 = [[0,47,39,75,67,8,42,53],[47,0,94,42,94,91,36,30],
  [39,94,0,91,48,37,25,1],[75,42,91,0,56,49,85,4],[67,94,48,56,0,64,44,40],
  [8,91,37,49,64,0,49,79],[42,36,25,85,44,49,0,55],[53,30,1,4,40,79,55,0]]

graph9 = [[0,47,39,75,67,8,42,53,87],[47,0,94,42,94,91,36,30,68],
  [39,94,0,91,48,37,25,1,93],[75,42,91,0,56,49,85,4,43],[67,94,48,56,0,64,44,40,81],
  [8,91,37,49,64,0,49,79,59],[42,36,25,85,44,49,0,55,28],[53,30,1,4,40,79,55,0,45],
  [87,68,93,43,81,59,28,45,0]]

graph10 = [[0,47,39,75,67,8,42,53,87,86],[47,0,94,42,94,91,36,30,68,70],
  [39,94,0,91,48,37,25,1,93,2],[75,42,91,0,56,49,85,4,43,73],[67,94,48,56,0,64,44,40,81,45],
  [8,91,37,49,64,0,49,79,59,89],[42,36,25,85,44,49,0,55,28,43],[53,30,1,4,40,79,55,0,45,35],
  [87,68,93,43,81,59,28,45,0,50],[86,70,2,73,45,89,43,35,50,0]]


graph20 = [[0,75,93,57,87,20,31,14,81,57,35,27,99,91,31],
  [75,0,20,38,30,9,26,32,30,51,17,92,43,55,8],
  [93,20,0,66,74,0,41,61,85,30,2,31,69,75,18],
  [57,38,66,0,39,38,13,92,54,78,37,70,9,47,75],
  [87,30,74,39,0,34,37,83,76,92,10,0,86,68,84],
  [20,9,0,38,34,0,33,4,13,36,38,66,17,99,60],
  [31,26,41,13,37,33,0,23,73,82,64,5,25,67,17],
  [14,32,61,92,83,4,23,0,25,26,69,6,32,50,61],
  [81,30,85,54,76,13,73,25,0,18,65,74,37,35,37],
  [57,51,30,78,92,36,82,26,18,0,83,98,1,61,62],
  [35,17,2,37,10,38,64,69,65,83,0,70,96,41,9],
  [27,92,31,70,0,66,5,6,74,98,70,0,92,83,82],
  [99,43,69,9,86,17,25,32,37,1,96,92,0,12,41],
  [91,55,75,47,68,99,67,50,35,61,41,83,12,0,96],
  [31,8,18,75,84,60,17,61,37,62,9,82,41,96,0]]

-----------------------------------------------


solve :: [[Int]] -> Int
solve g = tsp g (True : replicate (length g - 1) False)
   0 (length g) 1 0 (maxBound :: Int)


main = do
   putStrLn "Enter number of iterations:"
   times <- getLine
   let i = read times :: Integer
   putStrLn "Enter number of cities (4/5/8/9/10/20):"
   graph <- getLine
   let 
      g = read graph :: Integer
   a <- 
      (if g == 4 then time i graph4
         else if g == 5 then time i graph5
            else if g == 8 then time i graph8
               else if g == 9 then time i graph9
                  else if g == 10 then time i graph10
                     else time i graph20
      )
   
   putStrLn (intercalate "\n" a)
   putStrLn "Done, press enter to close ..."
   done <- getLine
   putStrLn done


time :: (Eq t, Num t) => t -> [[Int]] -> IO [String]
time x graph = if x == 0
   then
      return []
   else
      do
         a <- getCurrentTime
         let
            s = solve graph
         putStrLn ("Result: " ++ show s)
         b <- getCurrentTime
         let
            c = diffUTCTime b a
            s = 0
         d <- time (x-1) graph
         return (show c : d)
