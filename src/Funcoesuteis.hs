module Funcoesuteis where

import LI12122



--funcoes auxiliares para chao:

bloconext :: (Peca, Coordenadas) -> (Peca, Coordenadas) -> Bool
bloconext (a, (b, c))  (d, (e, f))
  |b == e && c == f-1 = True
  |b == e && c == f+1 = True
  |b == e-1 && c == f = True
  |b == e+1 && c == f = True
  |b == e+1 && c == f+1 = True
  |b == e-1 && c == f+1 = True
  |b == e+1 && c == f-1 = True
  |b == e-1 && c == f-1 = True
  |otherwise = False

--bloco mais abaixo da coluna
lowerblock :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
lowerblock [(p,(x,y))] = (p,(x,y))
lowerblock ((a, (x1, y1)) : (b, (x2, y2)) : t)
  |y1 >= y2 = lowerblock ((a, (x1, y1)) : t)
  |otherwise = lowerblock ((b, (x2, y2)) : t)

coluna :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
coluna n [] = []
coluna n ((p,(x,y)):t)
  |n == x = (p,(x,y)) : coluna n t
  |otherwise = coluna n t

isBloco :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
isBloco [] = []
isBloco ((p,(x,y)):t)
  |p == Bloco = (p,(x,y)) : isBloco t 
  |otherwise = isBloco t

isCaixa :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
isCaixa [] = []
isCaixa ((p,(x,y)):t)
  |p == Caixa = (p,(x,y)) : isCaixa t 
  |otherwise = isCaixa t 

box :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
box [] = (Bloco, (0,0))
box ((p,(x,y)):t) 
  |p == Caixa = (p,(x,y))  
  |otherwise = box t 

mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs


comprimento :: [(Peca, Coordenadas)] -> Int
comprimento [(a, (b, c))] = b
comprimento ((a, (b, c)) : (d, (e, f)) : xs)
  | b > e = comprimento ((a, (b, c)) : xs)
  | otherwise = comprimento ((d, (e, f)) : xs)

altura :: [(Peca, Coordenadas)] -> Int
altura [(a, (b, c))] = c
altura ((a, (b, c)) : (d, (e, f)) : xs)
  | c > f = altura ((a, (b, c)) : xs)
  | otherwise = altura ((d, (e, f)) : xs)

-- Tarefa3:
--deconstroi mapa da tarefa 2
decontroiMapa :: Mapa -> [(Peca, Coordenadas)]
decontroiMapa mapa = decontroiMapa' (0,0) mapa  

decontroiMapa' :: Coordenadas -> Mapa -> [(Peca, Coordenadas)]
decontroiMapa' (x,y) [[]]  = []
decontroiMapa' (x,y) ([]:xs)  = decontroiMapa' (0,y+1) xs 
decontroiMapa' (x,y) ((h:t) : xs)   
    |h == Vazio = decontroiMapa' (x+1,y) (t:xs) 
    |otherwise = (h,(x,y)) : decontroiMapa' (x+1,y) (t:xs)

insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 0 = elem : xs
    | pos > 0 = x : insertAt xs elem (pos - 1) 
    | otherwise = x : insertAt xs elem ((pos) + length (x:xs) )
