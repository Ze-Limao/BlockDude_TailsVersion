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
    |move == AndarDireita = if caminhobloqueadoD (Jogo mapa (Jogador (x,y) dir caixa)) == False then cair (Jogo mapa(Jogador (x+1,y) Este caixa)) (AndarDireita) else (Jogo mapa (Jogador (x,y) dir caixa))
    |move == AndarEsquerda = if caminhobloqueadoE (Jogo mapa (Jogador (x,y) dir caixa)) == False then cair (Jogo mapa(Jogador (x-1,y) Oeste caixa)) (AndarEsquerda) else (Jogo mapa (Jogador (x,y) dir caixa))
    |move == Trepar = (Jogo mapa (trepa (decontroiMapa mapa) (Jogador (x,y) dir caixa))) 
    |otherwise = (Jogo mapa (Jogador (x,y) dir caixa)) -- interagecaixa is Out of Service

caminhobloqueadoD :: Jogo -> Bool
caminhobloqueadoD (Jogo mapa (Jogador (x,y) dir caixa)) = camBloqD (decontroiMapa mapa) (Jogador (x,y) dir caixa)

camBloqD :: [(Peca,Coordenadas)] -> Jogador -> Bool
camBloqD [] (Jogador (x,y) dir caixa) = False
camBloqD ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(a == x+1 && b == y) && (h==Caixa || h==Bloco) = True
    |otherwise = camBloqD t (Jogador (x,y) dir caixa) 

caminhobloqueadoE :: Jogo -> Bool
caminhobloqueadoE (Jogo mapa (Jogador (x,y) dir caixa)) = camBloqE (decontroiMapa mapa) (Jogador (x,y) dir caixa)

camBloqE :: [(Peca,Coordenadas)] -> Jogador -> Bool
camBloqE [] (Jogador (x,y) dir caixa) = False
camBloqE ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(a == x-1 && b == y) && (h==Caixa || h==Bloco) = True
    |otherwise = camBloqE t (Jogador (x,y) dir caixa) 

cair :: Jogo -> Movimento -> Jogo
cair (Jogo mapa (Jogador (x,y) dir caixa)) move = (Jogo mapa (cair' (decontroiMapa mapa) (Jogador (x,y) dir caixa)))

cair' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
cair' l (Jogador (x,y) dir caixa)
    |(Jogador (x,y) dir caixa) /= cair'' l (Jogador (x,y) dir caixa) = cair' l (cair'' l (Jogador (x,y+1) dir caixa))
    |otherwise = (Jogador (x,y) dir caixa)

cair'' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
cair'' [] (Jogador (x,y) dir caixa) = (Jogador (x,y+1) dir caixa)
cair'' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==x && b == (y+1) = (Jogador (x,y) dir caixa)
    |otherwise = cair'' t (Jogador (x,y) dir caixa)

trepa :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepa [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepa ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |dir == Oeste = trepaEsq ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = trepaDir ((h,(a,b)):t) (Jogador (x,y) dir caixa)     

trepaEsq :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaEsq [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepaEsq ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
                |a==(x-1) && b==y = (Jogador (x-1,y-1) dir caixa)
                |otherwise = trepaEsq t (Jogador (x,y) dir caixa)

trepaDir :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaDir [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepaDir ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
                |a==(x+1) && b==y = (Jogador (x+1,y-1) dir caixa)
                |otherwise = trepaDir t (Jogador (x,y) dir caixa)        