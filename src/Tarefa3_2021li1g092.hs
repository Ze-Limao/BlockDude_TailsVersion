{- |
Module      : Tarefa3_2021li1g092
Description : Representação textual do jogo
Copyright   : Diogo José Correia Ribeiro <a100755@alunos.uminho.pt>;
            : José Afonso Lopes Correia <a100610@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g092 where

import LI12122
import Funcoesuteis

instance Show Jogo where
  show = undefined

-- :: Jogador -> Mapa -> String




mapa :: Mapa -> String
mapa [[]] = []
mapa [(h:t)] = mapa' (h:t) 
mapa ((h:t):tt) = mapa' (h:t) ++ "\n" ++ mapa tt

mapa' :: [Peca] -> String
mapa' [] = []
mapa' (h:t)  
  |h == Caixa = "C" ++ mapa' t 
  |h == Porta = "P" ++ mapa' t 
  |h == Bloco = "X" ++ mapa' t 
  |otherwise = " " ++ mapa' t 

