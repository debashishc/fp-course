-- syntax cheatsheet

x :: Integer
x = 99

f :: Integer -> Integer
f a = a + 10

g :: Integer -> Integer -> Integer
g a b = (a + b) * 2

h :: Integer -> Integer -> Integer
h = \a b -> (a + b) * 2

-- higher order functions
i :: (Integer -> Integer) -> Integer
i k = k 100 -- HOF

-- infix functions
--  char needs backticks : g a b <-> a `g` b
-- prefix functions
--  non-char needs parentheses : (.+.) a b <-> a .+. b 

-- polymorphism
j :: (Integer -> a) -> a
j k = k 100

z :: anything -> anything
z c = c

-- once inhabited types
y :: a -> b -> a
y     p    _  =  p

-- DATA TYPES
pie = 3
data Shape =      -- CLOSED algrbraic data type
  Circle Integer  --
  | Rectangle Integer Integer
  | Triangle Integer Integer Integer
  deriving (Show, Eq)

perimeter :: Shape -> Integer
perimeter = \s -> case s of
  Circle r        -> 2 * pie * r
  Rectangle w h   -> 2 * (w + h)
  Triangle a b c  -> a + b + c

perimeter2 :: Shape -> Integer
perimeter2 (Circle r)       = 2 * pie * r
perimeter2 (Rectangle w h)  = 2 * (w + h)
perimeter2 (Triangle a b c) = a + b + c

data Three a = 
  T a a a
  deriving (Eq, Show)

multiply :: Three Integer -> Integer
multiply (T a b c) = a * b * c

m :: (a -> b) -> Three a -> Three b
m = \q -> \ta -> case ta of
  T a1 a2 a3 -> T (q a1) (q a2) (q a3)

isInList :: (Eq a) => a -> [a] -> Bool
isInList e list = 
  case list of
    []      -> False
    (x:xs)  -> (e == x) || isInList e xs


data List a = 
  Nil | Cons a (List a)
  deriving (Eq, Show)

-- referencial transparency is preseverved
addList :: List Integer -> Integer
addList = \w  -> case w of
  Nil       ->  0
  Cons x xs ->  (+) x (addList xs)