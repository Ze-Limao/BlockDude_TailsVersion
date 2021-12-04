module Funcoesuteis where

import LI12122

{- |
@
comprimento [(a, (b, c))] = b
comprimento ((a, (b, c)) : (d, (e, f)) : xs)  
  | b > e = comprimento ((a, (b, c)) : xs)    
  | otherwise = comprimento ((d, (e, f)) : xs) 
@ -}
comprimento :: [(Peca, Coordenadas)] -> Int     
comprimento [(a, (b, c))] = b
comprimento ((a, (b, c)) : (d, (e, f)) : xs) 
  | b >= e = comprimento ((a, (b, c)) : xs)
  | otherwise = comprimento ((d, (e, f)) : xs)

{- | 
@
altura [(a, (b, c))] = c
altura ((a, (b, c)) : (d, (e, f)) : xs)
  | c >= f = altura ((a, (b, c)) : xs)
  | otherwise = altura ((d, (e, f)) : xs)
@
  -}
altura :: [(Peca, Coordenadas)] -> Int
altura [(a, (b, c))] = c
altura ((a, (b, c)) : (d, (e, f)) : xs)
  | c >= f = altura ((a, (b, c)) : xs)
  | otherwise = altura ((d, (e, f)) : xs)

{- |
@
lowerblock :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
lowerblock [(p,(x,y))] = (p,(x,y))
lowerblock ((a, (x1, y1)) : (b, (x2, y2)) : t)
  |y1 >= y2 = lowerblock ((a, (x1, y1)) : t)
  |otherwise = lowerblock ((b, (x2, y2)) : t)
@
-}
lowerblock :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
lowerblock [(p,(x,y))] = (p,(x,y))
lowerblock ((a, (x1, y1)) : (b, (x2, y2)) : t)
  |y1 >= y2 = lowerblock ((a, (x1, y1)) : t)
  |otherwise = lowerblock ((b, (x2, y2)) : t)

{-| 
@
coluna :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
coluna n [] = []
coluna n ((p,(x,y)):t)
  |n == x = (p,(x,y)) : coluna n t
  |otherwise = coluna n t
@
-}
coluna :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
coluna n [] = []
coluna n ((p,(x,y)):t)
  |n == x = (p,(x,y)) : coluna n t
  |otherwise = coluna n t
{-| 
@
isBloco :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
isBloco [] = []
isBloco ((p,(x,y)):t)
  |p == Bloco = (p,(x,y)) : isBloco t 
  |otherwise = isBloco t
@
-}
isBloco :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
isBloco [] = []
isBloco ((p,(x,y)):t)
  |p == Bloco = (p,(x,y)) : isBloco t 
  |otherwise = isBloco t
{-| 
@
isCaixa :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
isCaixa [] = []
isCaixa ((p,(x,y)):t)
  |p == Caixa = (p,(x,y)) : isCaixa t 
  |otherwise = isCaixa t 
@
-}
isCaixa :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
isCaixa [] = []
isCaixa ((p,(x,y)):t)
  |p == Caixa = (p,(x,y)) : isCaixa t 
  |otherwise = isCaixa t 
{-| 
@
box :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
box [] = (Bloco, (0,0))
box ((p,(x,y)):t) 
  |p == Caixa = (p,(x,y))  
  |otherwise = box t 
@
-}
box :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
box [] = (Bloco, (0,0))
box ((p,(x,y)):t) 
  |p == Caixa = (p,(x,y))  
  |otherwise = box t 
{-| 
@
mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs
@
-}
mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs


{- |
@
decontroiMapa :: Mapa -> [(Peca, Coordenadas)]
decontroiMapa mapa = decontroiMapa' (0,0) mapa 
@
-}
decontroiMapa :: Mapa -> [(Peca, Coordenadas)]
decontroiMapa mapa = decontroiMapa' (0,0) mapa  

{- |
@
decontroiMapa' :: Coordenadas -> Mapa -> [(Peca, Coordenadas)]
decontroiMapa' (x,y) [[]]  = []
decontroiMapa' (x,y) ([]:xs)  = decontroiMapa' (0,y+1) xs 
decontroiMapa' (x,y) ((h:t) : xs)   
    |h == Vazio = decontroiMapa' (x+1,y) (t:xs) 
    |otherwise = (h,(x,y)) : decontroiMapa' (x+1,y) (t:xs)
@
-}
decontroiMapa' :: Coordenadas -> Mapa -> [(Peca, Coordenadas)]
decontroiMapa' (x,y) [[]]  = []
decontroiMapa' (x,y) ([]:xs)  = decontroiMapa' (0,y+1) xs 
decontroiMapa' (x,y) ((h:t) : xs)   
    |h == Vazio = decontroiMapa' (x+1,y) (t:xs) 
    |otherwise = (h,(x,y)) : decontroiMapa' (x+1,y) (t:xs)

{- |
@
insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 0 = elem : xs
    | pos > 0 = x : insertAt xs elem (pos - 1) 
    | otherwise = x : insertAt xs elem ((pos) + length (x:xs) ) 
@
-}
insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 0 = elem : xs
    | pos > 0 = x : insertAt xs elem (pos - 1) 
    | otherwise = x : insertAt xs elem ((pos) + length (x:xs) )

linha :: Int -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
linha _ [] = []
linha n ((h,(a,b)):t) = if n==b then (h,(a,b)) : linha n t else linha n t

controimapa :: [(Peca,Coordenadas)] -> Mapa
controimapa [] = [[]]
controimapa ((h,(a,b)):t) = [controimapa'' (0,y) (linha y ((h,(a,b)):t)) | y <-[0..(altura((h,(a,b)):t))]]  

controimapa'' :: Coordenadas -> [(Peca,Coordenadas)] -> [Peca]
controimapa'' (n,m) [] = []
controimapa'' (n,m) ((h,(x,y)):t)
    |n==x = h : controimapa'' ((n+1),y) t 
    |otherwise = Vazio : controimapa'' ((n+1),y) ((h,(x,y)):t)

-- |yup escreve algo um certo numero de vezes

yup :: Int -> a -> [a]
yup 0 a = []
yup n a = a : yup (n-1) a 