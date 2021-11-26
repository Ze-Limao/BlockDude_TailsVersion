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
{-}
instance Show Jogo where
  show (Jogo mapa (Jogador (x,y) dir caixa))
    |show (Jogo [[]] (Jogador (x,y) dir caixa)) = "" 
    |otherwise = show mapa (Jogador (x,y) dir caixa)
                  |espacojogador mapa (Jogador (x,y) dir caixa) == False = ""
                  |espacojogador mapa (Jogador (x,y) dir caixa) == True = jogo' (mapear mapa) (Jogador (x,y) dir caixa)
-}

jogo :: Mapa -> Jogador -> String
jogo [[]] (Jogador (x,y) dir caixa) = "" 
jogo mapa (Jogador (x,y) dir caixa)
  |espacojogador mapa (Jogador (x,y) dir caixa) == False = ""
  |espacojogador mapa (Jogador (x,y) dir caixa) == True = jogo' (mapear mapa) (Jogador (x,y) dir caixa)

jogo' :: String -> Jogador -> String 
jogo' str (Jogador (x,y) dir caixa) = insertAt str (direcaodude (Jogador (x,y) dir caixa)) (posicaodude str (Jogador (x,y) dir caixa))

posicaodude :: String -> Jogador -> Int
posicaodude str (Jogador (x,y) dir caixa) = ((lengthX(lengthX' str)+1)  * y) + x + y

lengthX :: String -> Int
lengthX "" = 0
lengthX (h:t) = 1 + lengthX t

lengthX' :: String -> String
lengthX' "" = ""
lengthX' (h:t) 
  |h == '\n' = ""
  |otherwise = "a" ++ lengthX' t

direcaodude :: Jogador -> Char
direcaodude (Jogador (x,y) dir caixa) 
  |dir == Oeste = '<'
  |otherwise = '>'

espacojogador :: Mapa -> Jogador -> Bool
espacojogador mapa (Jogador (x,y) dir caixa) 
  |caixa == True = espacojogador' (decontroiMapa mapa) (Jogador (x,y-1) dir caixa) == 1 && espacojogador' (decontroiMapa mapa) (Jogador (x,y) dir caixa) == 1
  |otherwise = espacojogador' (decontroiMapa mapa) (Jogador (x,y) dir caixa) == 1

--tem de dar 1 para q o dude tenha espaco 
espacojogador' :: [(Peca,Coordenadas)] -> Jogador -> Int
espacojogador' [] j = 1
espacojogador' ((p,(a,b)):t) (Jogador (x,y) dir caixa)
  |a==x && b==y = 1 + espacojogador' t (Jogador (x,y) dir caixa)
  |otherwise = espacojogador' t (Jogador (x,y) dir caixa)


mapear :: Mapa-> String
mapear [[]] = ""
mapear [(h:t)] = mapear' (h:t) 
mapear ((h:t):tt) = mapear' (h:t) ++ "\n" ++ mapear tt

mapear' :: [Peca] -> String
mapear' [] = ""
mapear' (h:t)  
  |h == Caixa = "C" ++ mapear' t 
  |h == Porta = "P" ++ mapear' t 
  |h == Bloco = "X" ++ mapear' t 
  |otherwise = " " ++ mapear' t 

