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
    |otherwise = interagircaixa (Jogo mapa (Jogador (x,y) dir caixa)) 

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


-- |Funcao interagir caixa
interagircaixa :: Jogo -> Jogo
interagircaixa (Jogo mapa (Jogador (x,y) dir True)) = dropbox (Jogo mapa (Jogador (x,y) dir True))
interagircaixa (Jogo mapa (Jogador (x,y) dir False)) = (Jogo mapa (pickupbox (Jogo mapa (Jogador (x,y) dir False))))

dropBox :: Jogo -> Jogo
dropBox (Jogo mapa (Jogador (x,y) dir caixa)) 
    |dir == Oeste = dropWest (decontroiMapa mapa) (Jogador (x,y) dir caixa)
    |otherwise = dropEast (decontroiMapa mapa) (Jogador (x,y) dir caixa)

dropWest :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropWest [] (Jogador (x,y) dir caixa) = dropWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
dropWest ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(a==(x-1) && b==(y-1)= (Jogo mapa (Jogador (x,y) dir caixa)) --muro
    |(h==Porta && (a==(x-1) && b==y) = (Jogo mapa (Jogador (x,y) dir caixa)) --porta a frente
    |otherwise = dropWest t (Jogador (x,y) dir caixa)

dropWest' :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |dropWest'' == True = (Jogo (inserircaixa ))
    |otherwise = dropWest' ((h,(a,b)):t) (Jogador (x,y+1) dir caixa)

dropWest'' :: [(Peca,Coordenadas)] -> Jogador -> Bool
dropWest'' [] (Jogador (x,y) dir caixa) = False
dropWest'' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
        |y == altura((h,(a,b)):t) = False
        |(h==Bloco && (a==(x-1) && b==(y+1))) || (h==Caixa && (a==(x-1) && b==(y+1)) = True
        |otherwise = dropWest'' t (Jogador (x,y) dir caixa) 

inserircaixa

pickupbox :: Jogo -> Jogador
pickupbox (Jogo mapa (Jogador (x,y) dir False)) = if pick(Jogo mapa (Jogador (x,y) dir False)) == True then (Jogador (x,y) dir True) else (Jogador (x,y) dir False)

pick :: Jogo -> Bool --verifica se pode interagir
pick (Jogo mapa (Jogador (x,y) dir caixa)) 
    |dir == Oeste = pickWest (reverse(decontroiMapa mapa)) (Jogador (x,y) dir caixa)
    |otherwise = pickEast (reverse(decontroiMapa mapa)) (Jogador (x,y) dir caixa)

pickWest :: [(Peca,Coordenadas)] -> Jogador -> Bool
pickWest [] (Jogador (x,y) dir caixa) = False
pickWest ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |h==Caixa && (a==(x-1) && b==y)= pickWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = pickWest t (Jogador (x,y) dir caixa)

pickWest' :: [(Peca,Coordenadas)] -> Jogador -> Bool
pickWest' [] (Jogador (x,y) dir caixa) = True
pickWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==(x-1) && b==(y-1) = False
    |otherwise = pickWest' t (Jogador (x,y) dir caixa)

pickEast :: [(Peca,Coordenadas)] -> Jogador -> Bool
pickEast [] (Jogador (x,y) dir caixa) = False
pickEast ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |h==Caixa && (a==(x+1) && b==y)= pickEast' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = pickEast t (Jogador (x,y) dir caixa)

pickEast' :: [(Peca,Coordenadas)] -> Jogador -> Bool
pickEast' [] (Jogador (x,y) dir caixa) = True
pickEast' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==(x+1) && b==(y-1) = False
    |otherwise = pickEast' t (Jogador (x,y) dir caixa)