import Data.Char
import Data.List

-- 2
--a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t

--b)

numOcorre :: Char -> String -> Int
numOcorre _ "" = 0
numOcorre c (h:t)|(c == h) = 1 + numOcorre c t
				 |otherwise = numOcorre c t

--c)

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t)|(h >= 0) = positivos t

--d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)|(h >= 0) = h : soPos t
		   |otherwise = soPos t

--e)

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t)|(h >= 0) = somaNeg t
			 |otherwise = h + somaNeg t

--f)

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [x] = [x]
tresUlt [x,y] = [x,y]
tresUlt [x,y,z] = [x,y,z]
tresUlt (h:t) = tresUlt t

--g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t

--h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((x,y):t)|(a == x) = True
						|otherwise = nosPrimeiros a t

--i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+soma1, y+soma2,z+soma3)
                          where (soma1, soma2, soma3) = sumTriplos t

-- 3
-- a)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)|(isDigit h == True) = h : soDigitos t
               |otherwise = soDigitos t

--b)

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)|(isLower h) = 1 + minusculas t
                |otherwise= minusculas t

--c)

nums :: String -> [Int]
nums [] = []
nums (h:t)|(isDigit h) = digitToInt(h) : nums t
          |otherwise = nums t