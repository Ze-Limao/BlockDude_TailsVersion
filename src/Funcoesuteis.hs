module Funcoesuteis where
import LI12122


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
