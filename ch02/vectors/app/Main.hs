module Main where

import Data.Vector qualified as V

-- Creating a vectors
v :: V.Vector Int
v = V.fromList [1, 2, 3, 4, 5]

emptyVector :: V.Vector Int
emptyVector = V.empty

singletonVector :: V.Vector Int
singletonVector = V.singleton 1

tenOnes :: V.Vector Int
tenOnes = V.replicate 10 1

squaredVector :: V.Vector Int
squaredVector = V.map (^ 2) v

tenElementsSquared :: V.Vector Int
tenElementsSquared = V.generate 10 (^ 2)

-- Vector shares many list functions
fstElement :: Int
fstElement = V.head v

rest :: V.Vector Int
rest = V.tail v

lastElement :: Int
lastElement = V.last v

initElements :: V.Vector Int
initElements = V.init v

fstThreeElements :: V.Vector Int
fstThreeElements = V.take 3 v

lastTwoElements :: V.Vector Int
lastTwoElements = V.drop 3 v

sumOfVector :: Int
sumOfVector = V.sum v

sumOfVectorAgain :: Int
sumOfVectorAgain = V.foldl (+) 0 v

productOfVector :: Int
productOfVector = V.product v

vTwice :: V.Vector Int
vTwice = (V.++) v v

vectorOfVectors :: V.Vector (V.Vector Int)
vectorOfVectors = V.generate 5 (V.replicate 5)

main :: IO ()
main =
  putStrLn ""
    >> putStrLn "**********Vectors**********"
    >> print v
    >> print emptyVector
    >> print singletonVector
    >> print tenOnes
    >> print squaredVector
    >> print tenElementsSquared
    >> print fstElement
    >> print rest
    >> print lastElement
    >> print initElements
    >> print fstThreeElements
    >> print lastTwoElements
    >> print sumOfVector
    >> print sumOfVectorAgain
    >> print productOfVector
    >> print vTwice
    >> print vectorOfVectors
