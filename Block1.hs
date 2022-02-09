-- A function that greets people.
greet :: String -> String
greet name = "Hello " ++ name ++ "!"

greetTest :: Bool
greetTest =
  greet "Kofi" == "Hello Kofi!"
  && greet "Jeremy" == "Hello Jeremy!"
  && greet "" == "Hello !"

-- Functions that calculates the price of a cakes order.
cakeBill,cakeBill' :: Int -> Int -> String
cakeBill quantity price = "The cost of " ++ show quantity ++ " cakes at " ++ show price ++ "p each is " ++ show (quantity*price) ++ "p."
cakeBill' quantity price | quantity == 1 = "The cost of 1 cake at " ++ show price ++ "p each is " ++ show price ++ "p."
                         | otherwise     = "The cost of " ++ show quantity ++ " cakes at " ++ show price ++ "p each is " ++ show (quantity*price) ++ "p."

cakeBillTest,cakeBill'Test :: Bool
cakeBillTest =
  cakeBill 0 3 == "The cost of 0 cakes at 3p each is 0p."
  && cakeBill 1 3 == "The cost of 1 cakes at 3p each is 3p."
  && cakeBill 2 3 == "The cost of 2 cakes at 3p each is 6p."
cakeBill'Test =
  cakeBill' 0 3 == "The cost of 0 cakes at 3p each is 0p."
  && cakeBill' 1 3 == "The cost of 1 cake at 3p each is 3p."
  && cakeBill' 2 3 == "The cost of 2 cakes at 3p each is 6p."
  
-- A function that calculates the price of a bananas order.
bananas :: Int -> Int
bananas order | order < min_order    = error "Order size below minimum requirement."
              | order*price+p_and_p > reduction_price = order*price+reduced_p_and_p
              | otherwise            = order*price+op_and_p
              where
               min_order = 2
			   price = 300
			   p_and_p = 499
			   reduced_p_and_p = 349
			   reduction_price = 5000

bananasTest :: Bool
bananasTest =
  bananas 2 == 1099
  && bananas 20 == 6349

-- A function that converts pennies to pounds and pence.
pennies2pounds :: Int -> String
pennies2pounds pennies | pence < 10 = "£" ++ show pounds ++ ".0" ++ show pence
                       | otherwise  = "£" ++ show pounds ++ "." ++ show pence
                       where
                         (pounds, pence) = divMod pennies 100

pennies2poundsTest :: Bool
pennies2poundsTest =
  pennies2pounds 109 == "£1.09"
  && pennies2pounds 110 == "£1.10"

-- Functions to create the implication operator on Booleans.
implies,implies',implies'',implies''' :: Bool -> Bool -> Bool
implies x y = not x || y -- explicit arguments
implies' = (||) . not -- implicit arguments
implies'' True True = True -- full table
implies'' True False = False
implies'' False True = True
implies'' False False = True
implies''' True False = False -- using "don't care" patterns
implies''' _    _     = True

impliesTest,implies'Test,implies''Test,implies'''Test :: Bool
impliesTest =
  implies True True
  && not (implies True False)
  && implies False True
  && implies False False
implies'Test =
  implies' True True
  && not (implies' True False)
  && implies' False True
  && implies' False False
implies''Test =
  implies'' True True
  && not (implies'' True False)
  && implies'' False True
  && implies'' False False
implies'''Test =
  implies''' True True
  && not (implies''' True False)
  && implies''' False True
  && implies''' False False
  
-- Functions to check if you are in danger of an item eating another item.
data Item = Dog | Chicken | Grain deriving (Eq, Show)

eats :: Item -> [Item]
eats Dog = [Chicken]
eats Chicken = [Grain]
eats Grain = []

danger :: Item -> Item -> Bool
danger a b = a `elem` eats b || b `elem` eats a 

dangerTest :: Bool
dangerTest =
  danger Dog Chicken == True
  && danger Chicken Grain == True
  && danger Dog Grain == False
  
-- Functions that increases each element in a list by 1.
incList, incList' :: [Int] -> [Int]
incList [] = []
incList (n:ns) = n+1 : incList ns
incList' = map (+1)

incListTest,incList'Test :: Bool
incListTest = incList [42, 7, 16] == [43, 8, 17]
incList'Test = incList' [42, 7, 16] == [43, 8, 17]

-- An improved test for the greet function.
greetTest' :: [Bool]
greetTest' = map gt [("Kofi", "Hello Kofi!"), ("Jeremy", "Hello Jeremy!"), ("", "Hello !")]
           where
		     gt (input, expectedOutput) = greet input == expectedOutput
			 
-- A function that outputs the index of a given element
pos :: Eq a => a -> [a] -> Int
pos x = posx 0
  where
    posx n (y:ys) | x == y    = n
                  | otherwise = posx (n+1) ys

posTest :: Bool
posTest =
  pos 'h' "hello world" == 0
  && pos 'l' "hello world" == 2

-- A function that inserts a value into an ordered list.
insert :: Ord a => a -> [a] -> [a]
insert x = insx
  where
    insx [] = [x]
    insx ys@(z:zs) | x <= z    = x : ys
                   | otherwise = z : insx zs

insertTest :: Bool
insertTest =
  insert 3 [1,2,4,5] == [1,2,3,4,5]
  && insert 'c' "abde" == "abcde"
  && insert True [False] == [False, True]

-- A function that implements insertion sort.
isort :: Ord a => [a] -> [a]
isort = foldr insert []

isortTest :: Bool
isortTest =
  isort [9,8,7,6] == [6,7,8,9]
  && isort "hello" == "ehllo"
  && isort [True, False, True] == [False, True, True]

-- A fold function that inserts a value into an ordered list.
insert' :: Ord a => a -> [a] -> [a]
insert' x = foldr insx [x]
  where
    insx y ys@(z:zs) | z == x && x < y = z:y:zs
                     | otherwise       = y:ys

insert'Test :: Bool
insert'Test =
  insert' 3 [1,2,4,5] == [1,2,3,4,5]
  && insert' 'c' "abde" == "abcde"
  && insert' True [False] == [False, True]
  
-- A function that implements identity using foldr.
identity :: [a] -> [a]
identity = foldr (:) []

identityTest :: Bool
identityTest =
  identity [1,2,3] == [1,2,3]

-- A function that implements map using foldr.
mapAsRF :: (a -> b) -> [a] -> [b]
mapAsRF f = foldr ((:) . f) []

mapAsRFTest :: Bool
mapAsRFTest =
  mapAsRF greet ["Kofi", "Jeremy"] == ["Hello Kofi!", "Hello Jeremy!"]
  
-- Functions that reverse a list.
reVRF, revLF :: [a] -> [a]
revRF = foldr (\x xs -> xs ++ [x]) []
revLF = foldl (flip (:)) []

revRFTest, revLFTest :: Bool
revRFTest =
  revRF [1,2,3] == [3,2,1]
revLFTest =
  revLF [1,2,3] == [3,2,1]
  
-- Functions that output the length of a list.
lenRF, lenLF :: [a] -> Int
lenRF = foldr (const (1+)) 0
lenLF = foldl (flip (const (1+))) 0

lenRFTest, lenLFTest :: Bool
lenRFTest =
  lenRF "hello world" == 11
lenLFTest =
  lenLF "hello world" == 11
  
-- A function that implements the scalar multiplication of vectors.
type Vector = [Int]

(/*/) :: Int -> Vector -> Vector
(/*/) = map . (*)

smvTest :: Bool
smvTest =
  3 /*/ [1,1,1] == [3,3,3]

-- A function that implements the addition of vectors.
(/+/) :: Vector -> Vector -> Vector
(/+/) = undefined

(/+/)Test :: Bool
(/+/)Test = undefined

-- A function that implements a unit vector for (/+/)
zeroV :: Vector
zeroV = repeat 0

zeroVTest :: Bool
zeroVTest = undefined

-- A function that sums a list of vectors.
sumV :: [Vector] -> Vector
sumV = undefined

sumVTest :: Bool
sumVTest = undefined

-- A function that implements zipWith using explicit recursion.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = undefined

zipWith'Test :: Bool
zipWith'Test = undefined

-- A function that merges two ordered lists.
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined

mergeTest :: Bool
mergeTest = undefined

-- 6.15