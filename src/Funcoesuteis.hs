module Funcoesuteis where
import LI12122

--Tarefa 1: 

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

ultpeca :: [(Peca,Coordenadas)] -> Coordenadas
ultpeca [] = (0,0)
ultpeca [(p,(x,y))] = (x+1,y+1)
ultpeca ((p,(x,y)):t) 
    |x == comprimento [(p,(x,y))] && y == altura [(p,(x,y))] = (x,y)
    |otherwise = ultpeca t

area :: [(Peca,Coordenadas)] -> Int
area [] = 0
area l = area' (ultpeca l)

area' :: Coordenadas -> Int
area' (x,y) = x*y


nPecas :: [(Peca,Coordenadas)] -> Int
nPecas [] = 0
nPecas ((p,(x,y)):t) = 1 + nPecas t

nVazios :: [(Peca, Coordenadas)] -> Int
nVazios [] = 0
nVazios l = area l - nPecas l



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
  |x1 > x2 = lowerblock ((a, (x1, y1)) : t)
  |otherwise = lowerblock ((b, (x2, y2)) : t)

coluna :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
coluna n [] = []
coluna n ((p,(x,y)):t)
  |n == x = (p,(x,y)) : coluna n t
  |otherwise = coluna n t

linha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
linha n [] = []
linha n ((p,(x,y)):t)
  |n == y = (p,(x,y)) : linha n t
  |otherwise = linha n t

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


--[[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

--Tarefa 2:  
ordenaX :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaX [p,(x,y)] = [p,(x,y)]
ordenaX ((p1,(x1,y1)):(p2,(x2,y2)):t) 
  |x1<=x2 = (p1,(x1,y1)) : ordenaX ((p2,(x2,y2)):t)
  |x1 > x2 = ordenaX (ordenaX' (p1,(x1,y1)) ((p2,(x2,y2)):t)) 


ordenaX' :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaX' (p,(x,y)) [] = [(p,(x,y))]
ordenaX' (p,(x,y)) ((p1,(x1,y1)):(p2,(x2,y2)):t)
    |x <= x1 = (p,(x,y)) : ((p1,(x1,y1)):t)
    |x > x1 &&  x < x2 = (p1,(x1,y1)) : (p,(x,y)) : t
    |otherwise = (p,(x,y)) : ordenaX' (p,(x,y)) ((p2,(x2,y2)):t)



