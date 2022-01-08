{- |
Module      : Tarefa6_2021li1gXXX
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g092 where

import Tarefa4_2021li1g092
import Funcoesuteis 
import LI12122
import Data.Maybe


data Tree a = Node a [Tree a]


{- | __Função Principal que recebe um número de movimentos e um Jogo e devolverá se possivel uma lista com esse número de movimentos que resolvem um jogo.__ 

===Exemplo:

>>> resolveJogo 6 m1e1
Just [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda]
>>> resolveJogo 0 m1e1
Nothing

-}
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i jogo = resolveJogo' i (Node jogo [])
    where 
        resolveJogo' i a
            | i<0 = Nothing
            | getcaminho a /= Nothing = Just (converse $ fromJust $ getcaminho a)
            | otherwise = resolveJogo' (i-1) (addMove a)


-- |Recebe uma Tree e verifica se o Jogador já chegou à porta 
getcaminho :: Tree Jogo -> Maybe [Jogo] 
getcaminho (Node jogo []) = 
  if chegouPorta jogo then Just [jogo] 
                      else Nothing
getcaminho (Node jogo (h:t)) = 
  if getcaminho h /= Nothing then Just (jogo : fromJust (getcaminho h)) 
                             else getcaminho (Node jogo t)

-- |Verifica que se a coordenada do Jogador coincide com a Porta.
chegouPorta :: Jogo -> Bool 
chegouPorta (Jogo mapa (Jogador (x,y) dir caixa)) = ((mapa !! y) !! x) == Porta 


-- |Lista de movimentos possíveis.
movimentosPossivies :: [Movimento]
movimentosPossivies = [AndarEsquerda, AndarDireita, Trepar, InterageCaixa]

-- |Recebe uma Tree e expande a Tree. 
addMove :: Tree Jogo -> Tree Jogo
addMove (Node jogo ls@(_:_)) = Node jogo (addMove `map` ls)
addMove (Node jogo []) = Node jogo (addJogos jogo movimentosPossivies)

-- |Cria uma lista de jogos com os movimentos possíveis.
addJogos :: Jogo -> [Movimento] -> [Tree Jogo]
addJogos jogo ls =  (\x -> Node (moveJogador jogo x) []) `map` ls


-- |Converte uma lista de Jogos numa lista de Movimentos.
converse :: [Jogo] -> [Movimento]
converse [] = []
converse [_] = []
converse (x:y:t) = getMove (x,y) : converse (y:t)

-- |Devolve o movimento
getMove :: (Jogo,Jogo) -> Movimento
getMove ((Jogo mapa1 (Jogador (x1,y1) dir1 caixa1)),(Jogo mapa2 (Jogador (x2,y2) dir2 caixa2)))     
    |y1 == (y2+1) = Trepar
    |caixa1 /= caixa2 = InterageCaixa
    |dir2 == Este = AndarDireita 
    |otherwise = AndarEsquerda

