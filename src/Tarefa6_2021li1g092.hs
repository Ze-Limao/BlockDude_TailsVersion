{- |
Module      : Tarefa6_2021li1gXXX
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g092 where

import Tarefa3_2021li1g092
import Tarefa4_2021li1g092
import Funcoesuteis 
import LI12122
import Data.Maybe

data Tree a = Node a [Tree a]


m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
  
m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (2, 3) Oeste True)


movimentosPossivies :: [Movimento]
movimentosPossivies = [AndarEsquerda, AndarDireita, Trepar, InterageCaixa]


resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i jogo = resolveJogo' i (Node jogo [])
    where 
        resolveJogo' i a
            | i<0 = Nothing
            | getcaminho a /= Nothing = Just ( converse $ fromJust $ getcaminho a )
            | otherwise = resolveJogo' (i-1) (addMove a)


addMove :: Tree Jogo -> Tree Jogo
addMove (Node jogo ls@(_:_)) = Node jogo (addMove `map` ls)
addMove (Node jogo []) = Node jogo (addJogos jogo movimentosPossivies)


addJogos :: Jogo -> [Movimento] -> [Tree Jogo]
addJogos jogo ls =  (\x -> Node (moveJogador jogo x) []) `map` ls


converse :: [Jogo] -> [Movimento]
converse [] = []
converse [_] = []
converse (x:y:t) = getMove (x,y) : converse (y:t)


getMove :: (Jogo,Jogo) -> Movimento
getMove ((Jogo mapa1 (Jogador (x1,y1) dir1 caixa1)),(Jogo mapa2 (Jogador (x2,y2) dir2 caixa2)))     
    |y1 == (y2+1) = Trepar
    |caixa1 /= caixa2 = InterageCaixa
    |dir2 == Este = AndarDireita 
    |otherwise = AndarEsquerda


getcaminho :: Tree Jogo -> Maybe [Jogo] 
getcaminho (Node jogo []) = 
  if chegouPorta jogo then Just [jogo] 
                      else Nothing
getcaminho (Node jogo (h:t)) = 
  if getcaminho h /= Nothing then Just (jogo : fromJust (getcaminho h)) 
                             else getcaminho (Node jogo t)


chegouPorta :: Jogo -> Bool 
chegouPorta (Jogo mapa (Jogador (x,y) dir caixa)) = ((mapa !! y) !! x) == Porta 