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
import Tarefa2_2021li1g092
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


-- |Funcao interagir caixa (Esta funcao nao funciona para mapas com linhas vazias)
interagircaixa :: Jogo -> Jogo
interagircaixa (Jogo mapa (Jogador (x,y) dir True)) = dropBox (Jogo mapa (Jogador (x,y) dir True))
interagircaixa (Jogo mapa (Jogador (x,y) dir False)) = pickupbox (Jogo mapa (Jogador (x,y) dir False))

-- |Funcao drop box 

dropBox :: Jogo -> Jogo
dropBox (Jogo mapa (Jogador (x,y) dir caixa)) 
    |dir == Oeste = dropWest (decontroiMapa mapa) (Jogador (x,y) dir caixa)
    |otherwise = dropEast (decontroiMapa mapa) (Jogador (x,y) dir caixa)

dropWest :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropWest ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |auxW ((h,(a,b)):t) (Jogador (x,y) dir caixa) == True = dropWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = (Jogo (constroiMapa ((h,(a,b)):t)) (Jogador (x,y) dir caixa))

auxW :: [(Peca,Coordenadas)] -> Jogador -> Bool
auxW [] (Jogador (x,y) dir caixa) = True
auxW ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |(a==(x-1) && b==(y-1)) = False --muro
    |(h==Porta && (a==(x-1) && b==y)) = False --porta a frente
    |otherwise = auxW t (Jogador (x,y) dir caixa)

dropWest' :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa) = (Jogo (constroiMapa (inserircaixaW ((h,(a,b)):t) (Jogador (x,y) dir caixa))) (Jogador (x,y) dir False)) --inserir a caixa

inserircaixaW :: [(Peca,Coordenadas)] -> Jogador -> [(Peca,Coordenadas)]
inserircaixaW ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |y == ((altura ((h,(a,b)):t))+1) = ((h,(a,b)):t)
    |inserircaixaW' ((h,(a,b)):t) (Jogador (x,y) dir caixa) == True = ordenaC ((Caixa ,(x-1,y)):((h,(a,b)):t))
    |otherwise = inserircaixaW ((h,(a,b)):t) (Jogador (x,y+1) dir caixa)

inserircaixaW' :: [(Peca,Coordenadas)] -> Jogador -> Bool
inserircaixaW' [] (Jogador (x,y) dir caixa) = False
inserircaixaW' ((h,(a,b)):t) (Jogador (x,y) dir caixa)  
    |a==(x-1) && b==(y+1) = True
    |otherwise = inserircaixaW' t (Jogador (x,y) dir caixa)


dropEast :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropEast ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |auxE ((h,(a,b)):t) (Jogador (x,y) dir caixa) == True = dropEast' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = (Jogo (constroiMapa ((h,(a,b)):t))(Jogador (x,y) dir caixa))

auxE :: [(Peca,Coordenadas)] -> Jogador -> Bool
auxE [] (Jogador (x,y) dir caixa) = True
auxE ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |(a==(x+1) && b==(y-1)) = False --muro
    |(h==Porta && (a==(x+1) && b==y)) = False --porta a frente
    |otherwise = auxE t (Jogador (x,y) dir caixa)

dropEast' :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropEast' ((h,(a,b)):t) (Jogador (x,y) dir caixa) = (Jogo (constroiMapa (inserircaixaE ((h,(a,b)):t) (Jogador (x,y) dir caixa))) (Jogador (x,y) dir False)) --inserir a caixa

inserircaixaE :: [(Peca,Coordenadas)] -> Jogador -> [(Peca,Coordenadas)]
inserircaixaE ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |y == ((altura ((h,(a,b)):t))+1) = ((h,(a,b)):t)
    |inserircaixaE'((h,(a,b)):t) (Jogador (x,y) dir caixa) == True = ordenaC ((Caixa ,(x+1,y)):((h,(a,b)):t))
    |otherwise = inserircaixaE ((h,(a,b)):t) (Jogador (x,y+1) dir caixa)

inserircaixaE' :: [(Peca,Coordenadas)] -> Jogador -> Bool
inserircaixaE' [] (Jogador (x,y) dir caixa) = False
inserircaixaE' ((h,(a,b)):t) (Jogador (x,y) dir caixa)  
    |a==(x+1) && b==(y+1) = True
    |otherwise = inserircaixaE' t (Jogador (x,y) dir caixa)


-- |pickbox


pickupbox :: Jogo -> Jogo
pickupbox (Jogo mapa (Jogador (x,y) dir False)) = if pick(Jogo mapa (Jogador (x,y) dir False)) == True then (Jogo (removebox (decontroiMapa mapa) (Jogador (x,y) dir True)) (Jogador (x,y) dir True)) else (Jogo mapa (Jogador (x,y) dir False))

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


removebox :: [(Peca,Coordenadas)] -> Jogador -> Mapa
removebox ((h,(a,b)):t) (Jogador (x,y) Oeste caixa) = constroiMapa (removeW ((h,(a,b)):t) (Jogador (x,y) Oeste caixa))
removebox ((h,(a,b)):t) (Jogador (x,y) Este caixa) = constroiMapa (removeE ((h,(a,b)):t) (Jogador (x,y) Este caixa))


removeW :: [(Peca,Coordenadas)] -> Jogador -> [(Peca,Coordenadas)]
removeW ((h,(a,b)):(i,(c,d)):t) (Jogador (x,y) dir caixa)
    |a==x-1 && b==y = ((i,(c,d)): t)
    |otherwise = (h,(a,b)) : removeW ((i,(c,d)): t) (Jogador (x,y) dir caixa)

removeE :: [(Peca,Coordenadas)] -> Jogador -> [(Peca,Coordenadas)]
removeE ((h,(a,b)):(i,(c,d)):t) (Jogador (x,y) dir caixa)
    |a==x+1 && b==y = ((i,(c,d)): t)
    |otherwise = (h,(a,b)) : removeE ((i,(c,d)): t) (Jogador (x,y) dir caixa)