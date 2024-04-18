import Data.List
import Data.Function
import Data.Char

seguinte :: Int -> Int
seguinte x = x + 1

--1
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y = if x <= y  then x : enumFromTo (x+1) y else []
--2
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x y z = if x <= z then x : enumFromThenTo1 ((x+y)-1) y z else []
--3
concat1 :: Ord a => [a] -> [a] -> [a]
concat1 [] [] = []
concat1 (h:t) [] = (h:t)
concat1 (h:t) (m:n) = h : (t ++ (m:n))
--4
last1 :: [a] -> a 
last1 l = head (reverse l) 
--5
init1 :: [a] -> [a]
init1 [] = []
init1 l = reverse (tail(reverse l))  
--6
select :: [a] -> Int -> a
select [h] 0 = h
select (h:t) 0 = h 
select (h:t) x = if x > 0 then select t (x-1) else h
--7
reverse1 ::[a] -> [a]
reverse1 [] = []
reverse1 [x] = [x]
reverse1 (h:t) = (reverse1 t) ++ [h]
--8
take1 :: Int -> [a] -> [a]
take1 0 [a] = []
take1 1 [a,b] = [a]
take1 x [] = []
take1 x (h:t) = if x > 0 then h : (take1 (x-1) t) else []
--9
drop1 :: Int -> [a] -> [a]
drop1 0 [a] = [a]
drop1 1 [a] = []
drop1 1 [a,b] = [b]
drop1 x [] = []
drop1 x (h:t) = if x > 0 then drop1 (x-1) t else (h:t) 

--10 
zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] [] = []
zip1 (h:t) [] = []
zip1 [] (h:t) = []
zip1 [a] [b] = [(a,b)]
zip1 (h:t) (m:n) = (h,m) : (zip1 t n)

--11 
elem1 :: Eq a => a -> [a] -> Bool
elem1 a [] = False
elem1 a (h:t) = if a /= h
				then elem1 a t
				else True 

--12 
replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 1 x = [x]
replicate1 n x = if n >= 0
				 then x : (replicate1 (n-1) x)
        		 else []

--13
intersperce1 :: a -> [a] -> [a]
intersperce1 x [] = []
intersperce1 x [a] = [a]
intersperce1 x [a,b] = [a,x,b]
intersperce1 x (h:t) = h : x : (intersperce1 x t)

--14
group1 ::Eq a => [a] -> [[a]]
group1 [a] = [[a]]
group1 [] = [[]]
group1 t = repetidos t : group1 (retira (length (repetidos t)) t)

repetidos :: Eq a => [a] -> [a]
repetidos [a,b] = [a]
repetidos [] = []
repetidos (h:hs:t) = if h /= hs
					 then h : []
					 else h : (repetidos (h:t))

retira :: Int -> [a] -> [a]
retira 0 [x] = [x]
retira 1 [x] = []
retira x [] = []
retira n (h:t) = if n > 0 
				 then retira (n-1) t
				 else (h:t)				 
--15
concat2 ::[[a]] -> [a]
concat2 [[]] = []
concat2 (l :[]) = l
concat2 ((h:t):(m:n):l) = concat2 (((h:t) ++ (m:n)):l)

--16
inits1 ::[a] -> [[a]]
inits1 [] = [[]]
inits1 (h:t) = reverse1 (tails1 (h:t))


tails1 ::[a] -> [[a]]
tails1 [] = [[]]
tails1 (h:t) = (h:t) : tails1 (init1 (h:t))

--17
tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 (h:t) = (h:t) : tails2 t 

--18
isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 [] (h:t) = True
isPrefixOf1 (h:t) [] = False
isPrefixOf1 (h:t) (n:m) = if h == n
			     		  then isPrefixOf1 t m 
			     		  else False
--19
isSuffixOf1 :: Eq a => [a] -> [a] -> Bool
isSuffixOf1 [] (h:t) = True
isSuffixOf1 (h:t) [] = False
isSuffixOf1 l m = if last1 l == last1 m 
				  then isSuffixOf1 (init1 l) (init1 m)
				  else False 

--20
isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 (h:t) [] = False
isSubsequenceOf1 [] (h:t) = True
isSubsequenceOf1 (n:m) (h:t) =if n == h
				    		  then isSubsequenceOf1 m (h:t) 
				    		  else isSubsequenceOf1 (n:m) t
--21 
elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 a [] = []
elemIndices1 a (h:t) = contaElem a (h:t) 0

contaElem :: Eq a => a -> [a] -> Int -> [Int]
contaElem x [] p = []
contaElem x (h:t) p = if x == h 
					  then p : (contaElem x t (p+1))
					  else contaElem x t (p+1)


--22
nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (h:t) = h : nub1 (retira1 h (h:t)) 

retira1 :: Eq a => a -> [a] -> [a]
retira1 a [] = []
retira1 a (h:t) = if a == h
				  then retira1 a t 
		 	  	  else h : retira1  a t  
--23 
delete1 :: Eq a => a -> [a] -> [a]
delete1 a [] = []
delete1 a (h:t) = if a == h
				  then t
				  else h : delete1 a t
 
--24
deleteLista :: Eq a => [a] -> [a] -> [a]
deleteLista (h:t) [] = (h:t)
deleteLista (h:t) (x:xs) = if  verificaElem x (h:t) == False
			               then deleteLista t (x:xs) 
			               else deleteLista (delete1 x (h:t)) xs
--25 
union1 :: Eq a => [a] -> [a] -> [a]
union1 (h:t) [] = (h:t)
union1 [] (h:t) = []
union1 (h:t) (x:xs) = if verificaElem x (h:t) == True
					  then union1 (h:t) xs 
					  else (union1 (h:t) xs) ++ [x]

verificaElem ::Eq a => a -> [a] -> Bool
verificaElem a [] = False
verificaElem a (h:t) =if a == h
					  then True
					  else verificaElem a t 

--26
intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 (h:t) [] = []
intersect1 [] (h:t) = []
intersect1 (h:t) (x:xs) = if verificaElem h (x:xs) == True
						  then h : intersect1 t (x:xs)
						  else intersect1 t (x:xs) 
--27
insert1 :: Ord a => a -> [a] -> [a]
insert1 a [] = []
insert1 a (h:t) = if a >= h
				  then h : insert1 a t
				  else a : h : t
--28
maximum1 :: Ord a => [a] -> a 
maximum1 [a] = a
maximum1 (x:t) = if maiorElem x t == True
				 then x
			     else maximum1 t

maiorElem :: Ord a => a -> [a] -> Bool
maiorElem a [] = True
maiorElem a (h:t) =if a < h
					  then False
					  else maiorElem a t
--29 
minimum1 :: Ord a => [a] -> a 
minimum1 [a] = a 
minimum1 (x:t) = if menorElem x t == True
				 then x
				 else minimum1 t

menorElem :: Ord a => a -> [a] -> Bool
menorElem a [] = True
menorElem a (h:t) =if a > h
					  then False
					  else menorElem a t

--30
sum1 :: Num a => [a] -> a 
sum1 [a] = a
sum1 (h:t) = h + sum1 t

--31
product1 :: Num a => [a] -> a
product1 [a] = a
product1 (h:t) = h * product1 t 

--32
and1 :: [Bool] -> Bool
and1 [] = True
and1 (h:t) =if h == False
			then False
			else and1 t
--33
or1 :: [Bool] -> Bool
or1 [] = False
or1 (h:t) = if h == True
			then True
			else or1 t 
--34
unwords1 :: [String] -> String
unwords1 [(h:t)] = (h:t)
unwords1 ((h:t):(x:xs):l) = (h:t) ++ " " ++ unwords1 ((x:xs):l)

--35
unlines1 :: [String] -> String 
unlines1 [(h:t)] = (h:t)
unlines1 ((h:t):(x:xs):l) = (h:t) ++ "\n" ++ unlines1 ((x:xs):l) 

--36
pMaior1 :: Ord a => [a] -> Int
pMaior1 [a] = 0
pMaior1 (h:t) = if maiorElem h t == True
				then 0 
				else pMaior1 t + 1  
 

contaElem1 :: Eq a => a -> [a] -> Int -> Int
contaElem1 _[a]_ = 0
contaElem1 x (h:t) p = if x /= h 
					  then contaElem1 x t (p+1)
					  else p 
--37 
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = if verificaElem h t == True
				     then True
				     else temRepetidos t 
--38
algarismos :: [Char] -> [Char] 
algarismos [] = []
algarismos (h:t) = if isDigit h == True
				   then h : algarismos t 
				   else algarismos t 
				     
--39
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [a] = []
posImpares (x:xs:t) = xs : posImpares t
 
--40
posPares :: [a] -> [a]
posPares [] = []
posPares [a] = [a]
posPares (x:xs:t) = x : posPares t 

--41
isSorted1 :: Ord a => [a] -> Bool
isSorted1 [] = True
isSorted1 [a] = True
isSorted1 (x:xs:t) = if x > xs
				    then False
				    else isSorted1 (xs:t)

--42
iSort1 :: Ord a => [a] -> [a]
iSort1 [] = []
iSort1 (h:t) = insert h (iSort1 t)

--43
menor :: String -> String -> Bool
menor [] [] = True
menor [a] [] = False
menor [] [a] = True
menor (h:t) (x:xs) = if h <= x 
					 then menor t xs
					 else False
					  
--44
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a (h:t) = if fst h == a 
				   then True
				   else elemMSet a t 

--45
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet (h:t) = snd h + lengthMSet t	

--46 
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t) = convtPares (a,b)  ++ converteMSet t 			   

convtPares :: (a,Int) -> [a]
convtPares (a,0) = []
convtPares (a,b) = a : convtPares (a,(b-1))

--47
inseresMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
inseresMSet a [] = [(a,1)]
inseresMSet a ((x,xs):t) = if a == x 
						   then (x,(xs+1)):t
						   else (x,xs) : inseresMSet a t 

--48
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,xs):t) = if a == x 
						  then (x,(xs-1)) : removeMSet a t
						  else (x,xs) : removeMSet a t

--49
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (h,convtPares1 h (h:t)) : constroiMSet (retira (convtPares1 h (h:t)) (h:t))  

convtPares1 ::Eq a => a -> [a] -> Int
convtPares1 a [] = 0
convtPares1 a (h:t) = if a == h
					  then 1 + convtPares1 a t 
					  else 0     

--50 
somaPares :: [Int] -> Int
somaPares [] = 0
somaPares (h:t) = if mod h 2 == 0 
				  then h + somaPares t
				  else somaPares t
