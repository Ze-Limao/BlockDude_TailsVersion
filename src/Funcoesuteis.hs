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
  |y1 > y2 = lowerblock ((a, (x1, y1)) : t)
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


--vazios :
nVazios :: [(Peca, Coordenadas)] -> Int
nVazios [] = 0
nVazios l = area l - nPecas l

nPecas :: [(Peca,Coordenadas)] -> Int
nPecas [] = 0
nPecas ((p,(x,y)):t) = 1 + nPecas t

area :: [(Peca,Coordenadas)] -> Int
area [] = 0
area l = area' (ultpeca l)

area' :: Coordenadas -> Int
area' (x,y) = x*y

ultpeca :: [(Peca,Coordenadas)] -> Coordenadas
ultpeca [] = (0,0)
ultpeca [(p,(x,y))] = (x+1,y+1)
ultpeca ((p,(x,y)):t) 
    |x == comprimento [(p,(x,y))] && y == altura [(p,(x,y))] = (x,y)
    |otherwise = ultpeca t

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
