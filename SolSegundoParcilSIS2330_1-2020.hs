{-
1.	Dada una lista de números enteros, retornar en una lista los números que son primos.
 P.ejemplo: [6, 3, 7, 9, 21, 11]  la función retorna [3, 7, 11]
-}
{-
notación para reducir comentarios 
caso base => <b>
caso recursivo => <r>
bloque where (donde) para definiciones interna de funciones o otros argumentos => <w>	
-}

esPrimo::Int->Bool						
esPrimo n = esPrim n 2 								--llamamos a funcion que realmente hace el trabajo
		 where										--<w>
			esPrim n i|i > div n 2 = True			--<b> si mi contador es mayor n/2 NO encontraremos otro divisor por tanto es Primo 	
					  |mod n i == 0 = False			--<b> si encontramos un divisor NO es primo divisores [2..n/2]						
					  |otherwise = esPrim n (i+1)	--<r> otro caso solo desplazamos divisor i+1 al siguiente divisores					

listaPrimos::[Int]->[Int]
listaPrimos [] = [] 							--<b> si mi lista es vacia			
listaPrimos (x:xs)|esPrimo x = x:listaPrimos xs --<r> si primer valor(x) es primo se lo agrega al resultado 
				  |otherwise = listaPrimos xs 	--<r> si no es primo x no agregamos pero seguimos con el resto lista(xs)

{-	*** EJEMPLO EJECUCIÓN... ***

? listaPrimos [6, 3, 7, 9, 21, 11]
[3, 7, 11]
(139 reductions, 229 cells)

? listaPrimos [2..25]
[2, 3, 5, 7, 11, 13, 17, 19, 23]
(739 reductions, 1156 cells)
-}

{-
2.	Dado un número entero positivo, retornar en una lista la suma de los dígitos pares y
 el producto de los dígitos impares. P. ejemplo:  61235, la función retorna [8, 15]. La 
 lista resultante solo tiene dos elementos.
-}
operaDigitos::Int->[Int]
operaDigitos n = opeDig n 0 1												--definicion solo para claridad en tiempo de llamana 
			  where                                                         --<w>
			  	 opeDig 0 s m = [s,m]										--<b> si n=0 s=suma m=producto ... retornamos como lo expresado
			  	 opeDig n s m |even ultDig = opeDig (div n 10) (s+ultDig) m	--<r> si UlitmoDigito es par sumamos a s (suma) 
			  	 			  |otherwise = opeDig (div n 10) s (m*ultDig)	--<r> en otro caso (impar) multiplicamos a m (producto)
				  	 			where										--<w>			
				  	 				ultDig = mod n 10 						--definimos Ultimo Digito			

{-	*** EJEMPLO EJECUCIÓN... ***

? operaDigitos 61235
[8, 15]
(43 reductions, 89 cells)

? operaDigitos 123456789
[20, 945]
(70 reductions, 143 cells)
-}

{-
3.	Dada una lista de listas (matriz) de números enteros,  retornar un reporte que 
muestre la cantidad de números pares que hay en cada fila. 
P. ejemplo : [[5, 6, 2, 7, 1],[2, 9, 10, 3, 8],[8, 5, 2, 8, 1]], retorna:
En la fila 0 hay 2 números pares
En la fila 1 hay 3 números pares
En la fila 2 hay 3 números pares
-}
reporte::[[Int]]->Int->String
reporte [] _ = ""										--<b> si mi lista(lista de listas) e vacia retornamos cadena vacia (no imprimible) 																
reporte (xs:xss) i = "En la fila "++show(i)++" hay "++show(lisPar xs)++" pares\n"++reporte xss (i+1)
				where 									--<w> 
					lisPar [] = 0 						--<b> si mi lista es vacia no hay pares por tanto el resultado 0
					lisPar (x:xs)|even x = 1+lisPar xs 	--<r> sumamos +1 a nuestro resultado si primer elento(x) es par
								 |otherwise = lisPar xs --<r> caso contrario llamanos a funcion si nuestro primer elemento (el resto lista)
{-
para cada lista (xs) de las lista de listas, hacemos reporte de #fila (i) + #pares esa lista(lisPar xs) + llamada recursiva para pasar ala
siguiente lista de la lista de listas.

xs 		:primera lista de lista de listas
xss		:resto de la lista de listas sin el primer elemento (xs)
i 		:contador que debe empezar en 0 para 0,1,2..n-1 donde n es el tamaño de la lista de listas
show	:funcion para convertir argumento en representacion String
++		:operador para concatenar listas y Strings , por que un String no es mas que una lista de caracteres
\n 		:salto linea  	
-}

{-
ejemplo descripcion para esta funcion (lisPar)
[] = []
[1,2,3,4] = 1:[2,3,4] = lisPar [2,3,4]
[2,3,4] = 2:[3,4] = 2:lisPar [3,4]
[3,4] = 3:[4] = lisPar [4]
[4] = 4:[] = 4:lisPar []

[] = []
[1,2,3,4] = 1:[2,3,4] = [2,4]
[2,3,4] = 2:[3,4] = [2,4]
[3,4] = 3:[4] = [4]
[4] = 4:[] = [4]
-}

{-	*** EJEMPLO EJECUCIÓN... *** donde "0" es nuestro indice inicial

? reporte [[5, 6, 2, 7, 1],[2, 9, 10, 3, 8],[8, 5, 2, 8, 1]] 0
En la fila 0 hay 2 pares
En la fila 1 hay 3 pares
En la fila 2 hay 3 pares

(186 reductions, 496 cells)

? reporte [[5, 7, 1],[2,9,10,3,8],[8,5,2,8,12,14,16,18,1100,90,90,990,90]] 0
En la fila 0 hay 0 pares
En la fila 1 hay 3 pares
En la fila 2 hay 12 pares

(217 reductions, 550 cells)
-}

{-
4.	Dados dos números enteros positivos, retornar en una lista los dígitos que se 
encuentran en ambos numeros. P. ejemplo: 4725 y 2934, retorna [4,2]
-}

comun'::Int->Int->[Int]
comun' 0 m = []													--<b> si N=0 no tenemos mas que recorrer por tanto terminamos
comun' n m|esta (mod m 10) n = (mod m 10):comun' (div m 10) n 	--<r> si ultimo digito de "m" esta en el numero "n" lo agregamos a lista resul.
		  |otherwise = comun' (div m 10) n 						--<r> en otro caso seguimos con los siguientes digitos en m eliminando el ultimo dig.
		  where													--<w> (a continuación)funcion que verifica si un digito esta en un numero
			esta d 0 = False 									--<b> el nn=0 ya hay nada que hacer por que no hay mas digitos donde buscar
			esta d nn|d == (mod nn 10) = True					--<b> si digito "d" == al ultimo digito de "nn",encontramos el digito por tanto "d" esta en "nn"
					 |otherwise = esta d (div nn 10)   			--<r> en otro caso seguimos buscando eliminando el ultimo digito de "nn" con

{-	*** EJEMPLO EJECUCIÓN... ***

? comun' 4725 2934
[4, 2]
(96 reductions, 168 cells)

? comun' 123456789 987654321
[1, 9, 2, 8, 3, 7, 4, 6, 5]
(339 reductions, 595 cells)  

-}

--otra posible solucion...
{-
comun::Int->Int->[Int]
comun n m = inter (aLista m) (aLista n)							--solo esta linea es solucion si se toma en cuenta lo que se hizo en clases.
		where
			inter [] _ = []
			inter (x:xs) ys|elem x ys = x:inter xs ys
						   |otherwise = inter xs ys

			aLista n |n<10 = [n]
					 |otherwise = (mod n 10):aLista (div n 10)
-}