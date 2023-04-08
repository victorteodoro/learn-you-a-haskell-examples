-- Working with lists in Haskell

-- Lists are homogenous, meaning they can only contain one type of element
numberList = [1, 2, 3 ,4]
-- listError = ["1", 2, 3, 4] -- This will not compile

-- Lists can be nested
nestedList = [[1, 2, 3], [4, 5, 6]]

-- Lists can be concatenated with the ++ operator
concatenatedList = [1, 2, 3] ++ [4, 5, 6]

-- Lists can be prepended with the : operator
-- This is also called cons and is a lot faster than ++
-- because it doesn't have to traverse the entire list
prependedList = 1 : [2, 3, 4]

-- Lists can be accessed with the !! operator
-- This is also called indexing
third = [1, 2, 3, 4] !! 2
letterS = "Steve Buscemi" !! 8

-- Lists can be compared with the == operator
-- It compares element by element
equal = [1, 2, 3] == [1, 2, 3] -- True
notEqual = [1, 2, 3] == [1, 2, 4] -- False

-- Lists can be compared with the < operator
lessThan = [1, 2, 3] < [1, 2, 4] -- True

-- They can also be compared with the > operator
greaterThan = [1, 2, 3] > [1, 2, 4] -- False

-- How to access various parts of a list
first = head [1, 2, 3, 4]
rest = tail [1, 2, 3, 4]
allButLast = init [1, 2, 3, 4]
lastElement = last [1, 2, 3, 4]

-- How to check if a list is empty
isEmpty = null [1, 2, 3, 4] -- False
isStringEmpty = null "" -- True

-- How to get the length of a list
lengthOfList = length [1, 2, 3, 4]

-- How to reverse a list
reversed = reverse [1, 2, 3, 4]
steveReversed = reverse "Steve Buscemi"
steveAgain = reverse (reverse "Steve Buscemi")

-- How to take elements from a list
firstTwo = take 2 [1, 2, 3, 4]
firstThree = take 3 [1, 2, 3, 4]
firstTen = take 10 [1, 2, 3, 4]

-- Notice that it returns a list, not a single element
firstElement = head (take 1 [1, 2, 3, 4])

-- How to drop elements from a list
dropTwo = drop 2 [1, 2, 3, 4]

-- How to get the maximum and minimum element of a list
maxElement = maximum [1, 2, 3, 4]
minElement = minimum [1, 2, 3, 4]

-- To do it to just numbers, use `max` and `min`
maxOfTwo = max 4 5
minOfTwo = min 4 5

-- How to sum a list of numbers
sumOfList = sum [1, 2, 3, 4]

-- How to multiply a list of numbers
productOfList = product [1, 2, 3, 4]

-- How to check if an element is in a list
isInList = 4 `elem` [1, 2, 3, 4] -- True

-- How to create a list of numbers
oneToTwenty = [1..20]

-- How to create a list of characters
alphabet = ['a'..'z']

-- How to create a list of even numbers
-- Haskell deduces the step size from the first two elements
evenNumbers = [2, 4..20]

-- How to create a list of odd numbers
oddNumbers = [1, 3..20]

-- How to create a list of numbers in reverse
twentyToOne = [20, 19..1]

-- How to create an infinite list
-- This is possible because Haskell is lazy
infiniteList = [1..]
oneToTen = take 10 infiniteList

-- Using cycle to repeat a list
lol = take 15 (cycle "LOL ")

-- Using repeat to repeat an element
oneHundred = take 100 (repeat 1)

-- The same with replicate
oneHundredAgain = replicate 100 1

-- Creating lists with list comprehensions
listComp = [x * 2 | x <- [1..10]]
oddElements = [x | x <- [1..10], x `mod` 2 == 1]
oddElementsSimpler = [x | x <- [1..10], odd x]

-- You can use if expressions
fooBar = [if (x `mod` 3 == 0 || x `mod` 5 == 0) then (show x ++ " -> foo") else (show x ++ " -> bar") | x <- [1..100]]

-- You can have multiple sources and multiple predicates
cartesianProduct = [(x, y) | x <- [1..20], y <- [1..20], odd x, even y]

-- You can also discard the elements
length' xs = sum [1 | _ <- xs]

-- You can nest list comprehensions
listOfLists = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
nestedEvenElements = [[x | x <- xs, even x] | xs <- listOfLists]
