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
moveJogador (Jogo mapa (Jogador (x,y) dir caixa)) move 
    |move == AndarDireita = cair (Jogo mapa(Jogador (x+1,y) Este caixa)) (AndarDireita)
    |move == AndarEsquerda = cair (Jogo mapa(Jogador (x-1,y) Oeste caixa)) (AndarEsquerda)
    |move == Trepar = (Jogo mapa(Jogador (x-1,y-1) dir caixa)) 
    |otherwise = (Jogo mapa (Jogador (x,y) dir caixa))  --falta apenas tratar da interage caixa

cair :: Jogo -> Movimento -> Jogo
cair (Jogo mapa (Jogador (x,y) dir caixa)) move = (Jogo mapa (cair' (decontroiMapa mapa) (Jogador (x,y) dir caixa)))

cair' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
cair' l (Jogador (x,y) dir caixa)
    |(Jogador (x,y) dir caixa) /= cair'' l (Jogador (x,y) dir caixa) = cair' l (cair'' l (Jogador (x,y+1) dir caixa))
    |otherwise = cair'' l (Jogador (x,y) dir caixa)

cair'' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
cair'' [] (Jogador (x,y) dir caixa) = (Jogador (x,y+1) dir caixa)
cair'' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==x && b == (y+1) = (Jogador (x,y) dir caixa)
    |otherwise = cair'' t (Jogador (x,y) dir caixa)

