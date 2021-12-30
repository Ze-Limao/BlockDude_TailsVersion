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
  show (Jogo m j) = jogo m j
  
-- |__Função principal que escreve um Jogo numa String.__ 
jogo :: Mapa -> Jogador -> String
jogo [[]] (Jogador (x,y) dir caixa) = "" 
jogo mapa (Jogador (x,y) dir caixa)
  |espacojogador mapa (Jogador (x,y) dir caixa) == False = ""
  |espacojogador mapa (Jogador (x,y) dir caixa) == True = jogo' (mapear mapa) (Jogador (x,y) dir caixa)

-- |Função que insere o Jogador na string.
jogo' :: String -> Jogador -> String 
jogo' str (Jogador (x,y) dir False) = insertAt str (direcaodude (Jogador (x,y) dir False)) (posicaodude str (Jogador (x,y) dir False))
jogo' str (Jogador (x,y) dir True) = insertAt (jogo' str (Jogador (x,y) dir False)) 'C' (posicaodude str (Jogador (x,y-1) dir False))

-- |Função que nos diz onde o Jogador vai ser inserido na string.
posicaodude :: String -> Jogador -> Int
posicaodude str (Jogador (x,y) dir caixa) = ((lengthX (lengthX' str))  * y) + x + y

-- |Função que determina o comprimento de uma dada string.
lengthX :: String -> Int
lengthX "" = 0
lengthX (h:t) = 1 + lengthX t

-- |Função que determina o comprimento de uma linha do mapa.
lengthX' :: String -> String
lengthX' "" = ""
lengthX' (h:t) 
  |h == '\n' = ""
  |otherwise = "a" ++ lengthX' t

-- |Função que nos dá o o caracter do jogador consoante a direção.
direcaodude :: Jogador -> Char
direcaodude (Jogador (x,y) dir caixa) 
  |dir == Oeste = '<'
  |otherwise = '>'

-- |Função que verifica que o jogador com ou sem caixa consiga estar numa dada posição.
espacojogador :: Mapa -> Jogador -> Bool
espacojogador mapa (Jogador (x,y) dir caixa) 
  |caixa == True = espacojogador' (decontroiMapa mapa) (Jogador (x,y-1) dir caixa) == 1 && espacojogador' (decontroiMapa mapa) (Jogador (x,y) dir caixa) == 1
  |otherwise = espacojogador' (decontroiMapa mapa) (Jogador (x,y) dir caixa) == 1

-- |Dá 1 quando o Jogador tem espaço. 
espacojogador' :: [(Peca,Coordenadas)] -> Jogador -> Int
espacojogador' [] j = 1
espacojogador' ((p,(a,b)):t) (Jogador (x,y) dir caixa)
  |a==x && b==y = 1 + espacojogador' t (Jogador (x,y) dir caixa)
  |otherwise = espacojogador' t (Jogador (x,y) dir caixa)

-- |Escreve o mapa numa só string, em que \n indica o fim de cada linha.
mapear :: Mapa-> String
mapear [[]] = ""
mapear [(h:t)] = mapear' (h:t) 
mapear ((h:t):tt) = mapear' (h:t) ++ "\n" ++ mapear tt

-- |Função que escreve os respetivos caracteres de uma lista de peças.
mapear' :: [Peca] -> String
mapear' [] = ""
mapear' (h:t)  
  |h == Caixa = "C" ++ mapear' t 
  |h == Porta = "P" ++ mapear' t 
  |h == Bloco = "X" ++ mapear' t 
  |otherwise = " " ++ mapear' t 

