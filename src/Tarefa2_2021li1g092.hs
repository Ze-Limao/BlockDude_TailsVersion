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

{- | __Funçao que recebe uma lista de peças com as respetivas coordenadas e devolve um Mapa. Esta função é uma função com lista por compreensão.__ 

=== Exemplo:

>>> constroiMapa [(Porta,(0,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Caixa,(4,3)),(Bloco,(5,4)),(Bloco,(6,4)),(Bloco,(6,3)),(Bloco,(6,2)),(Bloco,(6,1))]
[[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
>>> constroiMapa []
[[]]
-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = [[]]
constroiMapa l@((h,(a,b)):t) = [constroiMapa' (comprimento l) (0,y) (linhaordenada y ((h,(a,b)):t)) | y <-[0..(altura((h,(a,b)):t))]]  

constroiMapa' :: Int -> Coordenadas -> [(Peca,Coordenadas)] -> [Peca]
constroiMapa' c (n,m) [] = yup c Vazio
constroiMapa' c (n,m) ((h,(x,y)):t) = constroiMapa'' c (n,m) ((h,(x,y)):t)

constroiMapa'' :: Int -> Coordenadas -> [(Peca,Coordenadas)] -> [Peca]
constroiMapa'' c (n,m) [] = []
constroiMapa'' c (n,m) ((h,(x,y)):t)
    |n==x = h : constroiMapa'' c ((n+1),m) t
    |otherwise = Vazio : constroiMapa'' c ((n+1),m) ((h,(x,y)):t)

-- | Ordena as uma linha de peças.
linhaordenada :: Int -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
linhaordenada _ [] = []
linhaordenada n ((h,(a,b)):t) = ordenaC (linhA n ((h,(a,b)):t))

-- | Seleciona as peças que pertençam a uma determinada linha (têm a mesma ordenada).
linhA :: Int -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
linhA _ [] = []
linhA n ((h,(a,b)):t) = if n==b then (h,(a,b)) : linhA n t else linhA n t

-- |Escreve algo um um certo numero de vezes +1, poisa primeira coluna ser a coluna 0.
yup :: Int -> a -> [a]
yup 0 a = [a]
yup n a = a : yup (n-1) a

{- | __Funçao que recebe um mapa e devolve uma lista de peças com as respetivas coordenadas. Esta função utilisa acumuladores.__

=== Exemplo:
>>> desconstroiMapa [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]] 
[(Bloco,(6,1)),(Bloco,(6,2)),(Porta,(0,3)),(Caixa,(4,3)),(Bloco,(6,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Bloco,(5,4)),(Bloco,(6,4))]
>>> desconstroiMapa [[]]
[]
 -}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapa' (0,0) mapa  

desconstroiMapa' :: Coordenadas -> Mapa -> [(Peca, Coordenadas)]
desconstroiMapa' (x,y) [[]]  = []
desconstroiMapa' (x,y) ([]:xs)  = desconstroiMapa' (0,y+1) xs 
desconstroiMapa' (x,y) ((h:t) : xs)   
    |h == Vazio = desconstroiMapa' (x+1,y) (t:xs) 
    |otherwise = (h,(x,y)) : desconstroiMapa'  (x+1,y) (t:xs)

