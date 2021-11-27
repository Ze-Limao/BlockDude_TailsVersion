{- |
Module      : Tarefa4_2021li1g092
Description : Movimentação do personagem
Copyright   : Diogo José Correia Ribeiro <a100755@alunos.uminho.pt>;
            : José Afonso Lopes Correia <a100610@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g092 where

import Funcoesuteis
import LI12122
import Tarefa3_2021li1g092

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (x:xs) = correrMovimentos (moveJogador jogo x) xs

moveJogador :: Jogo -> Movimento -> Jogo 
moveJogador (mapa (Jogador (x,y) Oeste caixa)) move = undefined

{-}
onemove :: Jogo -> Movimento -> Jogo 
onemove mapa (Jogador (x,y) dir caixa) move 
    |move == AndarDireita = (mapa(Jogador (x+1,y) Este caixa))
    |move == AndarEsquerda = (mapa(Jogador (x-1,y) Oeste caixa))
    |move == Trepar = (mapa(Jogador (x-1,y+1) dir caixa)) 
 --sss falta a interage caixa
-}

cair :: Jogo -> Movimento -> Jogo
cair mapa (Jogador (x,y) dir caixa) move
    |move == AndarEsquerda = cair' (decontroiMapa mapa) (Jogador (x-1,y) dir caixa)
    |otherwise = cair' (decontroiMapa mapa) (Jogador (x+1,y)) --ss


cair' :: [(Peca,Coordenadas)] -> Jogador -> Jogo
cair' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==x && b == (y+1) = (h,(a,b):t) (Jogador (x,y) dir caixa)
    |otherwise = cair' t (Jogador (x,y+1) dir caixa)
