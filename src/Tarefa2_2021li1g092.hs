{- |
Module      : Tarefa2_2021li1g092
Description : Construção/Desconstrução do mapa
Copyright   : Diogo José Correia Ribeiro <a100755@alunos.uminho.pt>;
            : José Afonso Lopes Correia <a100610@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g092 where

import LI12122
import Funcoesuteis

{- | __Funçao que recebe uma lista de peças com as respetivas coordenadas e devolve um Mapa.__ -}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = [[]]
constroiMapa ((h,(a,b)):t) = [constroiMapa'' (0,y) (linhA y ((h,(a,b)):t)) | y <-[0..(altura((h,(a,b)):t))]]  

constroiMapa'' :: Coordenadas -> [(Peca,Coordenadas)] -> [Peca]
constroiMapa'' (n,m) [] = []
constroiMapa'' (n,m) ((h,(x,y)):t)
    |n==x = h : constroiMapa'' ((n+1),m) t 
    |otherwise = Vazio : constroiMapa'' ((n+1),m) ((h,(x,y)):t)


linhA :: Int -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
linhA _ [] = []
linhA n ((h,(a,b)):t) = if n==b then (h,(a,b)) : linhA n t else linhA n t


{- | __Funçao que recebe um mapa e devolve uma lista de peças com as respetivas coordenadas.
    Esta função utilisa acumuladores.__ -}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapa' (0,0) mapa  

desconstroiMapa' :: Coordenadas -> Mapa -> [(Peca, Coordenadas)]
desconstroiMapa' (x,y) [[]]  = []
desconstroiMapa' (x,y) ([]:xs)  = desconstroiMapa' (0,y+1) xs 
desconstroiMapa' (x,y) ((h:t) : xs)   
    |h == Vazio = desconstroiMapa' (x+1,y) (t:xs) 
    |otherwise = (h,(x,y)) : desconstroiMapa'  (x+1,y) (t:xs)

