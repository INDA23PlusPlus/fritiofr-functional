to_lengths :: [String] -> [Int]
to_lengths [a] = [length a]
to_lengths (x:xs) = [length x] ++ to_lengths xs

quick_sort :: [Int] -> [Int]
quick_sort [] = []
quick_sort [a] = [a]
quick_sort [a, b] = if a < b then [a, b] else [b, a]
quick_sort (x:xs) = do
  let less_than = filter(\y -> y < x) xs
  let bigger_than = filter(\y -> y >= x) xs
  quick_sort less_than ++ [x] ++ quick_sort bigger_than

median_str_len :: [String] -> Int
median_str_len (xs) = do
  let s_xs = quick_sort (to_lengths xs)
  let len = length s_xs
  
  let lower_index = len `div` 2 - 1
  let higher_index = len `div` 2

  let higher = s_xs !! higher_index
  let lower = s_xs !! lower_index

  if len `mod` 2 == 0 
    then
      (higher + lower) `div` 2
    else 
      higher
