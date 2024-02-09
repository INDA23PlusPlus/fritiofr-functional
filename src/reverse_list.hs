res :: [a] -> [a]
res [x] = [x]
res (x:xs) = res xs ++ [x]

