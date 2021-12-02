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

-- | __Função que aplica uma lista de Movimentos a um Jogo, devolvendo o Jogo após as alterações impostas por os movimentos.
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (x:xs) = correrMovimentos (moveJogador jogo x) xs

-- | __Função que aplica apenas um Movimento a um Jogo, devolvendo o Jogo após as alterações impostas pelo Movimento.
moveJogador :: Jogo -> Movimento -> Jogo 
moveJogador (Jogo mapa (Jogador (x,y) dir caixa)) move 
    |move == AndarDireita = if caminhobloqueadoD (Jogo mapa (Jogador (x,y) dir caixa)) == False then cair (Jogo mapa(Jogador (x+1,y) Este caixa)) (AndarDireita) else (Jogo mapa (Jogador (x,y) dir caixa))
    |move == AndarEsquerda = if caminhobloqueadoE (Jogo mapa (Jogador (x,y) dir caixa)) == False then cair (Jogo mapa(Jogador (x-1,y) Oeste caixa)) (AndarEsquerda) else (Jogo mapa (Jogador (x,y) dir caixa))
    |move == Trepar = (Jogo mapa (trepa (decontroiMapa mapa) (Jogador (x,y) dir caixa))) 
    |otherwise = (Jogo mapa (Jogador (x,y) dir caixa)) --(Jogo mapa (interagircaixa (decontroiMapa mapa) (Jogador (x,y) dir caixa))) 

-- |Função que verifica se há espaço para avançar à direita.
caminhobloqueadoD :: Jogo -> Bool
caminhobloqueadoD (Jogo mapa (Jogador (x,y) dir caixa)) = camBloqD (decontroiMapa mapa) (Jogador (x,y) dir caixa)

-- |Função auxiliar que recebe a lista com as peças e verifica se existe alguma peça à direita do jogador.
camBloqD :: [(Peca,Coordenadas)] -> Jogador -> Bool
camBloqD [] (Jogador (x,y) dir caixa) = False
camBloqD ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(a == x+1 && b == y) && (h==Caixa || h==Bloco) = True
    |otherwise = camBloqD t (Jogador (x,y) dir caixa) 

-- |Função que verifica se há espaço para avançar à esquerda.
caminhobloqueadoE :: Jogo -> Bool
caminhobloqueadoE (Jogo mapa (Jogador (x,y) dir caixa)) = camBloqE (decontroiMapa mapa) (Jogador (x,y) dir caixa)

-- |Função auxiliar que recebe a lista com as peças e verifica se existe alguma peça à esquerda do jogador.
camBloqE :: [(Peca,Coordenadas)] -> Jogador -> Bool
camBloqE [] (Jogador (x,y) dir caixa) = False
camBloqE ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(a == x-1 && b == y) && (h==Caixa || h==Bloco) = True
    |otherwise = camBloqE t (Jogador (x,y) dir caixa) 

-- |Função que vai verificar se um Jogador tem algo para apoiar os pés.
cair :: Jogo -> Movimento -> Jogo
cair (Jogo mapa (Jogador (x,y) dir caixa)) move = (Jogo mapa (cair' (decontroiMapa mapa) (Jogador (x,y) dir caixa)))

-- |Função que vai fazer o jogador cair sempre que este não tenha uma peça por baixo.
cair' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
cair' l (Jogador (x,y) dir caixa)
    |(Jogador (x,y) dir caixa) /= cair'' l (Jogador (x,y) dir caixa) = cair' l (cair'' l (Jogador (x,y+1) dir caixa))
    |otherwise = (Jogador (x,y) dir caixa)

-- |Função que procura a peça imediatamente abaixo do jogador.
cair'' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
cair'' [] (Jogador (x,y) dir caixa) = (Jogador (x,y+1) dir caixa)
cair'' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==x && b == (y+1) = (Jogador (x,y) dir caixa)
    |otherwise = cair'' t (Jogador (x,y) dir caixa)

-- |Função que calcula o Movimento Trepar caso o obstaculo esteja a esquerda ou à direita do Jogador.
trepa :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepa [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepa ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |dir == Oeste = trepaEsq (reverse((h,(a,b)):t)) (Jogador (x,y) dir caixa)
    |otherwise = trepaDir (reverse((h,(a,b)):t)) (Jogador (x,y) dir caixa)     

-- |Função que calcula o Movimento Trepar à esquerda do Jogador.
trepaEsq :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaEsq [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepaEsq ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(h==Bloco && (a==(x-1) && b==y )) || (h==Caixa && (a==(x-1) && b==y))= trepaEsq' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = trepaEsq t (Jogador (x,y) dir caixa)

trepaEsq' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaEsq' [] (Jogador (x,y) dir caixa) = (Jogador (x-1,y-1) dir caixa)
trepaEsq' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==(x-1) && b==(y-1) = (Jogador (x,y) dir caixa)
    |otherwise = trepaEsq' t (Jogador (x,y) dir caixa) 

-- |Função que calcula o Movimento Trepar à direita do Jogador.
trepaDir :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaDir [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepaDir ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(h==Bloco && (a==(x+1) && b==y )) || (h==Caixa && (a==(x+1) && b==y))= trepaDir' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = trepaDir t (Jogador (x,y) dir caixa)

trepaDir' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaDir' [] (Jogador (x,y) dir caixa) = (Jogador (x+1,y-1) dir caixa)
trepaDir' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==(x+1) && b==(y-1) = (Jogador (x,y) dir caixa)
    |otherwise = trepaDir' t (Jogador (x,y) dir caixa)     
{-
interagircaixa :: Jogo -> Jogo

}
interact :: Jogo -> Bool --verifica se pode interagir
interact (Jogo mapa (Jogador (x,y) dir caixa)) 
    |dir == Oeste = interactWest (Jogo mapa (Jogador (x,y) dir caixa))
    |otherwise = interactEast (Jogo mapa (Jogador (x,y) dir caixa))


boxWest :: (Peca, Coordenadas) -> Jogador -> Bool
boxWest (a, (b, c))  (Jogador (x,y) dir caixa)
  |b == e && c == f-1 = True
-}