module Main where

-- Implementing map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Implementing foldl
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

-- Implementing filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

-- Implementing quicksort using filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter' (<=x) xs)
      biggerSorted  = quicksort (filter' (>x) xs)
  in  smallerSorted ++ [x] ++ biggerSorted



main :: IO ()
main = 
    putStrLn ""
    >> print ("******* Using the functions *******")
    >> print (map' (+1) [1,2,3])
    >> print (foldl' (+) 0 [1,2,3])
    >> print (((filter' (\x -> x `mod` 3 == 0)) . (filter' odd)) [1..50])
    >> print (quicksort [50, 47 .. 1])
    >> putStrLn ""