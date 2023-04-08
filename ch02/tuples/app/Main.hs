module Main where

-- How to create a tuple
tuple :: (Integer, Integer)
tuple = (1, 2)

-- How to create a list of tuples
-- Tuples can contain elements of different types
tupleList :: [(Integer, String)]
tupleList = [(1, "2"), (3, "4"), (5, "6")]

-- How to get the first element of a tuple
firstElement :: Integer
firstElement = fst (1, 2)

-- How to get the second element of a tuple
secondElement :: Integer
secondElement = snd (1, 2)

-- How to zip two lists together
-- The resulting list will have the same length as the shortest list
zipped :: [(Integer, String)]
zipped = zip [1 ..] ["apple", "orange", "cherry", "mango"]

-- Which right triangle that has integers for all sides and all
-- sides equal to or smaller than 100 has a perimeter between
-- 36 and 50?
-- (a, b, c)
rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles =
  [ (a, b, c)
    | c <- [1 .. 100],
      b <- [1 .. c],
      a <- [1 .. b],
      a ^ 2 + b ^ 2 == c ^ 2,
      a + b + c >= 36,
      a + b + c <= 50
  ]

main :: IO ()
main =
  putStrLn ""
    >> putStrLn "**********Tuples**********"
    >> print tuple
    >> print tupleList
    >> print firstElement
    >> print secondElement
    >> print zipped
    >> print rightTriangles
