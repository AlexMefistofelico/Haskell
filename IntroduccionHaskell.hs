module SIS233OA where
cuadrado::Int->Int
cuadrado n = n*n

mcd::Int->Int->Int
mcd a b |a==0 = b
		|otherwise = mcd (mod b a) a
--Polimorfa...
fact n |n==0 = 1
	   |otherwise = n* fact (n-1)

fibo::Int->Int
fibo n |n==0||n==1 = 1
       |otherwise = fibo (n-1) + fibo (n-2)

-------------------------------------
contDig::Int->Int
contDig n |n < 10 = 1
		  |otherwise = 1+contDig (div n 10)

contDigPar::Int->Int
contDigPar n |n == 0 = 0
		  	 |mod (mod n 10) 2 == 0 = 1+contDigPar (div n 10)
		  	 |otherwise = contDigPar (div n 10)

estaEnNUm::Int->Int->Bool
estaEnNUm n d |n == 0 = False
		  	  |(mod n 10) == d = True
		  	  |otherwise = estaEnNUm (div n 10) d

esPrim::Int->Int->Bool
esPrim n i|i > div n 2 = True
		   |mod n i == 0 = False
		   |otherwise = esPrim n (i+1)

esPrimo::Int->Bool
esPrimo n = esPrim n 2

-------------------------------------------
-- + where
-- patron...

--M es mi mayor dig
--N es mi numero...

mayDig::Int->Int->Int
mayDig n m |n==0 = m
           |(mod n 10)> m = mayDig (div n 10) (mod n 10)  --n%10
           |otherwise = mayDig (div n 10) m

repDig::Int->Int->Int
repDig n d |n==0 = 0
           |ultDig == d = 1 + repDig (div n 10) d
           |otherwise = repDig (div n 10) d
           where
           		ultDig = (mod n 10)

preg1::Int->String
preg1 n = "mayor: "++ show(mayDig n 0) ++ " y se repite: "++ show(repDig n digMayor)++" veces..."
		where
			digMayor = mayDig n 0

--vec[i] == vec!!i

tamLista::[Int]->Int
tamLista [] = 0 --caso base
tamLista (pElem:rLista) = 1 + tamLista rLista --caso recursivo
{-
[] = 0 
[1,2,3] = 1:[2,3] = 1 + tamLista [2,3]
[2,3] = 2:[3] = 1 + tamLista [3]
[3] = 3:[] = 1 + tamLista []
-}
{-
[] = 0 
[1,2,3] = 1:[2,3] = 3
[2,3] = 2:[3] = 2
[3] = 3:[] = 1
-}
sumLista::[Int]->Int
sumLista [] = 0 --caso base
sumLista (pElem:rLista) = pElem + sumLista rLista --caso recursivo
{-
[3,3,5] = 3:[3,5] = 3 + sumLista [3,5]
[3,5] = 3:[5] = 3 + sumLista [5]
[5] = 5:[] = 5 + sumLista []
-}
{-
[3,3,5] = 3:[3,5] = 11
[3,5] = 3:[5] = 8
[5] = 5:[] = 5
-}
prodLista::[Int]->Int
prodLista [] = 1 --caso base
prodLista (pElem:rLista) = pElem * prodLista rLista --caso recursivo

listaPares::[Int]->[Int]
listaPares [] = [] --caso base
listaPares (pElem:rLista)|even pElem = pElem:listaPares rLista --caso recursivo
						 |otherwise = listaPares rLista --caso recursivo

{-
[] = []
[1,2,3,4] = 1:[2,3,4] = listaPares [2,3,4]
[2,3,4] = 2:[3,4] = 2:listaPares [3,4]
[3,4] = 3:[4] = listaPares [4]
[4] = 4:[] = 4:listaPares []

[] = []
[1,2,3,4] = 1:[2,3,4] = [2,4]
[2,3,4] = 2:[3,4] = [2,4]
[3,4] = 3:[4] = [4]
[4] = 4:[] = [4]
-}

listaPrimos::[Int]->[Int]
listaPrimos [] = [] --caso base
listaPrimos (pElem:rLista)	|esPrimo pElem = pElem:listaPrimos rLista --caso recursivo
				   		 	|otherwise = listaPrimos rLista --caso recursivo

inv::Int->Int->Int
inv 0 r = r --caso base 
inv n r = inv (div n 10) (r*10+(mod n 10))

inver::Int->Int
inver n = inv n 0

omirp::Int->String
omirp n |esPrimo n && esPrimo nInver = "es omirp por que "++ show(n)++ " es primo y " ++ show(nInver) ++" son primos"
		|otherwise = show ( n)++" no es primo "
		where
			nInver = inver n
{-
n == 0 => r
123 0 = inv 12 3
12 3 = inv 1 32
1 32 = inv 0 321
0 321 = 321
-}

tramo::[Int]->[Int]
tramo [] = [] --caso base
tramo (pElem:sElem:rLista)|pElem<sElem=pElem:tramo (sElem:rLista)
						  |otherwise = [pElem]

{-
[2,23,2,23]
[] == []

tramo [2,23,2,23] = (2:23:[2,23]) = 2:tramo [23,2,23]
tramo [23,2,23] = (23:2:[23]) = [23]

tramo [2,23,2,23] = (2:23:[2,23]) = [2,23]
tramo [23,2,23] = (23:2:[23]) = [23]
-}

val::Char->Int
val 'I' = 1
val 'V' = 5
val 'X' = 10
val 'L' = 50
val 'C' = 100
val 'D' = 500
val 'M' = 1000

arabigo::String->Int
arabigo [x] = val x
arabigo (x:y:xs) |val x < val y = -val x + arabigo (y:xs)
				 |otherwise = val x + arabigo (y:xs)
{-
[x] = val x --caso base
arabigo ['C','I','V'] = arabigo ('C':'I':['V']) = 100 + arabigo ('I':['V'])
arabigo ['I','V'] = arabigo ('I':'V':[]) = -1 + arabigo ['V']
arabigo ['V'] = 5
-}
{-
arabigo ['C','I','V'] = arabigo ('C':'I':['V']) = 104
arabigo ['I','V'] = arabigo ('I':'V':[]) = 44
arabigo ['V'] = 5
-}
--CXI
--CIV

--123 -> [3,2,1]

reverso::[Int]->[Int]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

--[] == []
{-
reverso [1,2,3] = (1:[2,3]) = reverso [2,3] ++ [1]
reverso [2,3] = (2:[3]) = reverso [3] ++ [2]
reverso [3] = (3:[]) = reverso [] ++ [3]

reverso [1,2,3] = (1:[2,3]) = [3,2,1]
reverso [2,3] = (2:[3]) = [3,2]
reverso [3] = (3:[]) = [3]

c = 1
[1,2,3]
[1,9,3]
[3,1,6]
-}

aLista::Int->[Int]
aLista n |n<10 = [n]
		 |otherwise = (mod n 10):aLista (div n 10)
						
sumaLista::Int->Int->[Int]
sumaLista n m = reverso (sumList (aLista n) (aLista m) 0)
				where
					sumList [] [] 0 = []
					sumList [] [] c = [c]
					sumList xs [] _ = xs
					sumList [] ys _ = ys
					sumList (x:xs) (y:ys) c = ((x+y+c) `mod` 10):sumList xs ys ((x+y+c) `div` 10)


filtrar::[Int]->(Int->Bool)->[Int]
filtrar [] _ = []
filtrar (x:xs) f|f x= x:filtrar xs f
				|otherwise = filtrar xs f


{-
1.-En Gofer/Haskell. Dada dos lista de valores enteros. retornar en una lista los valores de la primera lista que no están en la segunda lista Ej.
[6,2,8,3,9,11,5] [2,1,9,4]		Retorna: [6,8,3,11,5]
-}

diff::[Int]->[Int]->[Int]
diff [] _ = []
diff (x:xs) ys|noEsta x ys = x:diff xs ys
			  |otherwise = diff xs ys
			  where
			  	noEsta _ [] = True
				noEsta m (x:xs)|m==x=False
							   |otherwise = noEsta m xs
{-
2.-	ya realizado
-}

{-
3.- En Gofer/Haskell Dada una lista de lista (matriz) retornar en una lista de listas unicamente los numeros pares. Ej.
[[6,2,8,3],[2,1,9,4],[1,9,7,2],[6,2,9,1]]	Retorna: [[6,2,8],[2,4],[2],[6,2]]
-}

filtMatriz::[[Int]]->[[Int]]
filtMatriz [] = []
filtMatriz (xs:xss) = filtrar xs even:filtMatriz xss
{-++++++++++++++++++++++++++++++++++++++++++++++++++-}
{-
1.-En Gofer/Haskell. Dada dos matrices con la misma cantidada de las filas, retornar en una lista de listas los elementos comunes de ambas matrices
por filas Ej. [[6,2,8,3],[2,1,9,4],[1,9,7,2],[6,2,9,1]] [[8,3,6],[1,6,2],[4,8,3],[2,7,9]] -> [[6,8,3],[2,1],[],[2,9]]
-}
inter::[Int]->[Int]->[Int] -- interseccion de listas..
inter [] _ = []
inter (x:xs) ys|elem x ys = x:inter xs ys
			   |otherwise = inter xs ys

comun::[[Int]]->[[Int]]->[[Int]]
comun (xs:xss) (ys:yss) = inter xs ys:comun xss yss 
					
{-
2.- ya realizado
-}

{-
3.-	en practica
-}


------PLUS
filtrarMatriz::[[Int]]->String
filtrarMatriz xss = "\n"++comoMatriz xss ++ "\nFiltrando Pares:\n\n"++ comoMatriz(filtMatriz xss)
				where
					comoMatriz xss = unlines (comMat xss)
								where
									comMat [] = []
									comMat (xs:xss) = show xs:comMat xss

