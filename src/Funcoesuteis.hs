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
    | otherwise = x : insertAt xs elem ((pos) + length (x:xs)) 

-- |ordena listas de pecas da mesma linha.

{- |
@
ordenaC :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordenaC [] = []
ordenaC ((h1,(a1,b1)):t) = ordenaC' (h1,(a1,b1)) (ordenaC t) 

ordenaC' :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordenaC' (h1,(a1,b1)) [] = [(h1,(a1,b1))]
ordenaC' (h1,(a1,b1)) ((h2,(a2,b2)):t)
  |a1 > a2 = (h2,(a2,b2)) : ordenaC' (h1,(a1,b1)) t
  |otherwise = (h1,(a1,b1)) : (h2,(a2,b2)) : t
-}
ordenaC :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordenaC [] = []
ordenaC ((h1,(a1,b1)):t) = ordenaC' (h1,(a1,b1)) (ordenaC t) 

ordenaC' :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordenaC' (h1,(a1,b1)) [] = [(h1,(a1,b1))]
ordenaC' (h1,(a1,b1)) ((h2,(a2,b2)):t)
  |a1 > a2 = (h2,(a2,b2)) : ordenaC' (h1,(a1,b1)) t
  |otherwise = (h1,(a1,b1)) : (h2,(a2,b2)) : t