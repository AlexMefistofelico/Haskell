--         __________   __________   __________   __________   ________
--        /  _______/  /  ____   /  /  _______/  /  _______/  /  ____  \
--       /  / _____   /  /   /  /  /  /______   /  /______   /  /___/  /
-- AACC./  / /_   /  /  /   /  /  /  _______/  /  _______/  /  __   __/
--     /  /___/  /  /  /___/  /  /  /         /  /______   /  /  \  \
--    /_________/  /_________/  /__/         /_________/  /__/    \__\
--Funciones Booleanas: --------------------------------------------------
(&&)
(||)
not
and
or
any
all
otherwise
--Funciones Para Caracteres: --------------------------------------------
isAscii
isControl
isPrint
isSpace
isUpper
isLower
isAlpha
isDigit
isAlphanum
isAscii   -- c < 128
isControl
isPrint
isSpace
isUpper
isLower
isAlpha
isDigit
isAlphanum
toUpper
toLower
minChar                --0
maxChar                --255
-- Clases tipo Estandar: ------------------------------------------------
max
min
range
index
inRange

enumFrom        -- [n..]
enumFromThen    -- [n,m..]
enumFromTo      -- [n..m]
enumFromThenTo  -- [n,n'..m]

negate
fromInteger

pi
{- PC version on -}
subtract
even
odd
gcd
lcm
abs
signum
sum
product

sums
products
-- Funciones Estandar para Procesamiento de Listas: ---------------------
head
last
tail
init
genericLength
length
iterate
repeat
cycle
copy
nub
reverse
elem
notElem
maximum
minimum
concat
transpose
null
(\\)
map
filter
--  foldl f a [x1, x2, ..., xn]  = f (...(f (f a x1) x2)...) xn
--                               = (...((a `f` x1) `f` x2)...) `f` xn
foldl
foldl1
foldl'
scanl

scanl1
scanl'
foldr
foldr1
scanr
scanr1
-- Funciones de Fraccionamiento de Listas: ------------------------------
take
drop
splitAt
takeWhile
takeUntil
dropWhile
span
break
-- Procesamiento de Texto: ----------------------------------------------
lines
words
unlines
unwords
-- Mescla y Orden de Listas: --------------------------------------------
merge
sort
insert
qsort
-- Familia de funciones zip y zipWith: ----------------------------------
zip
zip3
zip4
zip5
zip6
zip7

zipWith
zipWith3
zipWith4
zipWith5
zipWith6
zipWith7
unzip
--
curry
uncurry
fst
snd
fst3
snd3
thd3
(.)
flip
($)
-- Formateo De Salida: --------------------------------------------------
show'
cjustify
ljustify
rjustify
space
layn
-- Miscelánea: ----------------------------------------------------------
until
until'
asTypeOf
-- Un Version Corta de la clase Text de Haskell: ------------------------
show
showChar
showString
-- Algunas Funciones y Definiciones I/O : -------------------------------
print
prints
