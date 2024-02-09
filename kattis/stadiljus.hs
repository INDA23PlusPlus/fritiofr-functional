import Data.List

intDivExact :: Integer -> Integer -> Float
intDivExact x y =
  let 
    a = fromIntegral x :: Float
    b = fromIntegral y :: Float
  in
    a / b


data TestCase = TestCase {
  n :: Integer,
  x :: Integer,
  y :: Integer,
  k :: [Integer]
} deriving Show

solve :: TestCase -> [Integer]
solve (TestCase n x y k) = [r_solve values_queue 0 0]
  where
    max_always_allowed = y `intDivExact` x
    values_queue = sort k

    r_solve :: [Integer] -> Integer ->  Integer -> Integer; 
    r_solve [] a b = b
    r_solve list aggregate amount = 
      let
        (head:tail) = list
        new_aggregate = aggregate + head
        new_amount = amount + 1
        curr_mean = new_aggregate `intDivExact` new_amount
      in
        if curr_mean > max_always_allowed then
          amount
        else
          r_solve tail new_aggregate new_amount


parseInput :: String -> TestCase
parseInput input = 
  let
    (k_raw:rest) = reverse $ lines input
    [n, x, y] = reverse $ map read rest
    k = map read $ words k_raw
  in
    TestCase { n = n, x = x, y = y, k = k }

main = interact (writeOutput . solve . parseInput)

writeOutput = unlines . (map show)
