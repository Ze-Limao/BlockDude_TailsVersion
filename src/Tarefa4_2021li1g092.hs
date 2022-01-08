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

{- | __Função que aplica uma lista de Movimentos a um Jogo, devolvendo o Jogo após as alterações impostas por os movimentos.

===Exemplo:

>>> correrMovimentos (Jogo[[Bloco,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Caixa, Vazio, Caixa, Vazio, Bloco],[Bloco,Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (6,4) Oeste False)) [Trepar, AndarDireita, InterageCaixa]
X      X
XP     X
XX     X
XX  C  X
XX  <C X
XXXXXXXX

-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (x:xs) = correrMovimentos (moveJogador jogo x) xs

{- | __Função que aplica apenas um Movimento a um Jogo, devolvendo o Jogo após as alterações impostas pelo Movimento.

===Exemplo:
>>> moveJogador (Jogo[[Bloco,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Caixa, Vazio, Caixa, Vazio, Bloco],[Bloco,Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (6,4) Oeste False))  InterageCaixa
X      X
XP     X
XX     X
XX    CX
XX C  <X
XXXXXXXX

-}
moveJogador :: Jogo -> Movimento -> Jogo 
moveJogador (Jogo mapa (Jogador (x,y) dir caixa)) move 
    |move == AndarDireita = if caminhobloqueadoD (Jogo mapa (Jogador (x,y) dir caixa)) == False then cair (Jogo mapa(Jogador (x+1,y) Este caixa)) (AndarDireita) else (Jogo mapa (Jogador (x,y) Este caixa))
    |move == AndarEsquerda = if caminhobloqueadoE (Jogo mapa (Jogador (x,y) dir caixa)) == False then cair (Jogo mapa(Jogador (x-1,y) Oeste caixa)) (AndarEsquerda) else (Jogo mapa (Jogador (x,y) Oeste caixa))
    |move == Trepar = (Jogo mapa (trepa (desconstroiMapa mapa) (Jogador (x,y) dir caixa))) 
    |otherwise = interagircaixa (Jogo mapa (Jogador (x,y) dir caixa)) 

-- |Função que verifica se há espaço para avançar à direita.
caminhobloqueadoD :: Jogo -> Bool
caminhobloqueadoD (Jogo mapa (Jogador (x,y) dir caixa)) = camBloqD (desconstroiMapa mapa) (Jogador (x,y) dir caixa)

-- |Função auxiliar que recebe a lista com as peças e verifica se existe alguma peça à direita do jogador ou um vazio.
camBloqD :: [(Peca,Coordenadas)] -> Jogador -> Bool
camBloqD [] (Jogador (x,y) dir caixa) = False
camBloqD ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |comprimento ((h,(a,b)):t) == x = True
    |(a == x+1 && b == y) && (h==Caixa || h==Bloco) = True
    |otherwise = camBloqD t (Jogador (x,y) dir caixa) 

-- |Função que verifica se há espaço para avançar à esquerda.
caminhobloqueadoE :: Jogo -> Bool
caminhobloqueadoE (Jogo mapa (Jogador (x,y) dir caixa)) = camBloqE (desconstroiMapa mapa) (Jogador (x,y) dir caixa)

-- |Função auxiliar que recebe a lista com as peças e verifica se existe alguma peça à esquerda do jogador ou um vazio.
camBloqE :: [(Peca,Coordenadas)] -> Jogador -> Bool
camBloqE [] (Jogador (x,y) dir caixa) = False
camBloqE ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |x==0 = True
    |(a == x-1 && b == y) && (h==Caixa || h==Bloco) = True
    |otherwise = camBloqE t (Jogador (x,y) dir caixa) 

-- |Função que vai verificar se um Jogador tem algo para apoiar os pés.
cair :: Jogo -> Movimento -> Jogo
cair (Jogo mapa (Jogador (x,y) dir caixa)) move = (Jogo mapa (cair' (desconstroiMapa mapa) (Jogador (x,y) dir caixa)))

-- |Função que vai fazer o jogador cair sempre que este não tenha uma peça por baixo.
cair' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
cair' l (Jogador (x,y) dir caixa)
    |(Jogador (x,y) dir caixa) /= cair'' l (Jogador (x,y) dir caixa) = cair' l (cair'' l (Jogador (x,y+1) dir caixa))
    |otherwise = (Jogador (x,y) dir caixa)

-- |Função que procura a peça imediatamente abaixo do jogador.
cair'' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
cair'' [] (Jogador (x,y) dir caixa) = (Jogador (x,y+1) dir caixa)
cair'' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(h==Caixa || h==Bloco) && (a==x && b == (y+1)) = (Jogador (x,y) dir caixa)
    |otherwise = cair'' t (Jogador (x,y) dir caixa)

-- |Função que calcula o Movimento Trepar caso o obstaculo esteja a esquerda ou à direita do Jogador.
trepa :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepa [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepa ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |dir == Oeste = trepaEsq (reverse((h,(a,b)):t)) (Jogador (x,y) dir caixa)
    |otherwise = trepaDir (reverse((h,(a,b)):t)) (Jogador (x,y) dir caixa)     

-- |Função que  verifica se é possível trepar, calcula o Movimento trepar à esquerda do Jogador.
trepaEsq :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaEsq [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepaEsq ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(h==Bloco && (a==(x-1) && b==y )) || (h==Caixa && (a==(x-1) && b==y)) = trepaEsq' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = trepaEsq t (Jogador (x,y) dir caixa)

trepaEsq' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaEsq' [] (Jogador (x,y) dir caixa) = (Jogador (x-1,y-1) dir caixa)
trepaEsq' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(h==Caixa || h==Bloco) && ((a==(x-1) && b==(y-1))) = (Jogador (x,y) dir caixa) --caso não seja possivel trepar
    |otherwise = trepaEsq' t (Jogador (x,y) dir caixa) 

-- |Função que verifica se é possível trepar, calcula o Movimento trepar à direita do Jogador.
trepaDir :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaDir [] (Jogador (x,y) dir caixa) = (Jogador (x,y) dir caixa)
trepaDir ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(h==Bloco && (a==(x+1) && b==y )) || (h==Caixa && (a==(x+1) && b==y))= trepaDir' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = trepaDir t (Jogador (x,y) dir caixa)

trepaDir' :: [(Peca,Coordenadas)] -> Jogador -> Jogador
trepaDir' [] (Jogador (x,y) dir caixa) = (Jogador (x+1,y-1) dir caixa)
trepaDir' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |(h==Caixa || h==Bloco) && (a==(x+1) && b==(y-1)) = (Jogador (x,y) dir caixa) --caso não seja possivel trepar
    |otherwise = trepaDir' t (Jogador (x,y) dir caixa)     


-- |Funcao interagir caixa, que fará dropbox caso o Jogador carregue uma caixa,caso contrário fará pickupbox.
interagircaixa :: Jogo -> Jogo
interagircaixa (Jogo mapa (Jogador (x,y) dir True)) = dropBox (Jogo mapa (Jogador (x,y) dir True))
interagircaixa (Jogo mapa (Jogador (x,y) dir False)) = pickupbox (Jogo mapa (Jogador (x,y) dir False))

-- |Função dropbox 
dropBox :: Jogo -> Jogo
dropBox (Jogo mapa (Jogador (x,y) dir caixa)) 
    |dir == Oeste = dropWest (desconstroiMapa mapa) (Jogador (x,y) dir caixa)
    |otherwise = dropEast (desconstroiMapa mapa) (Jogador (x,y) dir caixa)

-- |Função que vai colocar uma caixa à frente do Jogador quando este está virado para Oeste
dropWest :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropWest ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |auxW ((h,(a,b)):t) (Jogador (x,y) dir caixa) == True = dropWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = (Jogo (constroiMapa ((h,(a,b)):t)) (Jogador (x,y) dir caixa))

-- |Auxiliar que verifica que não existem obstaculos à esquerda. 
auxW :: [(Peca,Coordenadas)] -> Jogador -> Bool
auxW [] (Jogador (x,y) dir caixa) = True
auxW ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |(a==(x-1) && b==(y-1)) = False --muro
    |(h==Porta && (a==(x-1) && b==y)) = False --porta a frente
    |otherwise = auxW t (Jogador (x,y) dir caixa)

-- |Função que trata de inserir a caixa no mapa.
dropWest' :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa) = (Jogo (constroiMapa (inserircaixaW ((h,(a,b)):t) (Jogador (x,y) dir caixa))) (Jogador (x,y) dir False)) --inserir a caixa

inserircaixaW :: [(Peca,Coordenadas)] -> Jogador -> [(Peca,Coordenadas)]
inserircaixaW ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |y == ((altura ((h,(a,b)):t))+1) = ((h,(a,b)):t) --caso de paragem
    |inserircaixaW' ((h,(a,b)):t) (Jogador (x,y) dir caixa) == True = ordenaC ((Caixa ,(x-1,y-1)):((h,(a,b)):t)) 
    |otherwise = inserircaixaW ((h,(a,b)):t)  (Jogador (x,y+1) dir caixa)

-- |Obstáculo à frente
inserircaixaW' :: [(Peca,Coordenadas)] -> Jogador -> Bool
inserircaixaW' [] (Jogador (x,y) dir caixa) = False
inserircaixaW' ((h,(a,b)):t) (Jogador (x,y) dir caixa)  
    |a==(x-1) && b==(y) = True
    |otherwise = inserircaixaW' t (Jogador (x,y) dir caixa)

-- |Função que vai colocar uma caixa à frente do Jogador quando este está virado para Este
dropEast :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropEast ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |auxE ((h,(a,b)):t) (Jogador (x,y) dir caixa) == True = dropEast' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = (Jogo (constroiMapa ((h,(a,b)):t))(Jogador (x,y) dir caixa))

-- |Auxiliar que verifica que não existem obstaculos à direita. 
auxE :: [(Peca,Coordenadas)] -> Jogador -> Bool
auxE [] (Jogador (x,y) dir caixa) = True
auxE ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |(a==(x+1) && b==(y-1)) = False --muro
    |(h==Porta && (a==(x+1) && b==y)) = False --porta a frente
    |otherwise = auxE t (Jogador (x,y) dir caixa)

-- |Função que trata de inserir a caixa no mapa.
dropEast' :: [(Peca,Coordenadas)] -> Jogador -> Jogo
dropEast' ((h,(a,b)):t) (Jogador (x,y) dir caixa) = (Jogo (constroiMapa (inserircaixaE ((h,(a,b)):t) (Jogador (x,y) dir caixa))) (Jogador (x,y) dir False)) --inserir a caixa

inserircaixaE :: [(Peca,Coordenadas)] -> Jogador -> [(Peca,Coordenadas)]
inserircaixaE ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |y == ((altura ((h,(a,b)):t))+1) = ((h,(a,b)):t) --caso de paragem
    |inserircaixaE'((h,(a,b)):t) (Jogador (x,y) dir caixa) == True = ordenaC ((Caixa ,(x+1,y-1)):((h,(a,b)):t))
    |otherwise = inserircaixaE ((h,(a,b)):t) (Jogador (x,y+1) dir caixa)

-- |Obstáculo à frente
inserircaixaE' :: [(Peca,Coordenadas)] -> Jogador -> Bool
inserircaixaE' [] (Jogador (x,y) dir caixa) = False
inserircaixaE' ((h,(a,b)):t) (Jogador (x,y) dir caixa)  
    |a==(x+1) && b==(y) = True
    |otherwise = inserircaixaE' t (Jogador (x,y) dir caixa)


-- |Função pickupbox
pickupbox :: Jogo -> Jogo
pickupbox (Jogo mapa (Jogador (x,y) dir False)) = 
    if pick (Jogo mapa (Jogador (x,y) dir False)) == True then (Jogo (removebox (desconstroiMapa mapa) (Jogador (x,y) dir True)) (Jogador (x,y) dir True)) 
                                                          else (Jogo mapa (Jogador (x,y) dir False))

-- |Verifica se pode interagir à direita e à esquerda
pick :: Jogo -> Bool 
pick (Jogo mapa (Jogador (x,y) dir caixa)) 
    |dir == Oeste = pickWest (reverse(desconstroiMapa mapa)) (Jogador (x,y) dir caixa)
    |otherwise = pickEast (reverse(desconstroiMapa mapa)) (Jogador (x,y) dir caixa)

-- | Verifica se existe uma caixa à esquerda do Jogador.
pickWest :: [(Peca,Coordenadas)] -> Jogador -> Bool
pickWest [] (Jogador (x,y) dir caixa) = False
pickWest ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |h==Caixa && (a==(x-1) && b==y) = pickWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = pickWest t (Jogador (x,y) dir caixa)

-- | Verificar se existem obstáculos em cima da caixa. 
pickWest' :: [(Peca,Coordenadas)] -> Jogador -> Bool
pickWest' [] (Jogador (x,y) dir caixa) = True
pickWest' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==(x-1) && b==(y-1) = False
    |otherwise = pickWest' t (Jogador (x,y) dir caixa)

-- | Verifica se existe uma caixa à direita do Jogador.
pickEast :: [(Peca,Coordenadas)] -> Jogador -> Bool
pickEast [] (Jogador (x,y) dir caixa) = False
pickEast ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |h==Caixa && (a==(x+1) && b==y)= pickEast' ((h,(a,b)):t) (Jogador (x,y) dir caixa)
    |otherwise = pickEast t (Jogador (x,y) dir caixa)

-- | Verificar se existem obstáculos em cima da caixa. 
pickEast' :: [(Peca,Coordenadas)] -> Jogador -> Bool
pickEast' [] (Jogador (x,y) dir caixa) = True
pickEast' ((h,(a,b)):t) (Jogador (x,y) dir caixa) 
    |a==(x+1) && b==(y-1) = False
    |otherwise = pickEast' t (Jogador (x,y) dir caixa)

-- | Função que devolve o mapa sem a caixa comque o jogador interage.
removebox :: [(Peca,Coordenadas)] -> Jogador -> Mapa
removebox ((h,(a,b)):t) (Jogador (x,y) Oeste caixa) = constroiMapa (removeW ((h,(a,b)):t) (Jogador (x,y) Oeste caixa))
removebox ((h,(a,b)):t) (Jogador (x,y) Este caixa) = constroiMapa (removeE ((h,(a,b)):t) (Jogador (x,y) Este caixa))

-- |Remove à esquerda.
removeW :: [(Peca,Coordenadas)] -> Jogador -> [(Peca,Coordenadas)]
removeW ((h,(a,b)):(i,(c,d)):t) (Jogador (x,y) dir caixa)
    |a==x-1 && b==y = ((i,(c,d)): t)
    |otherwise = (h,(a,b)) : removeW ((i,(c,d)): t) (Jogador (x,y) dir caixa)

-- |Remove à direita.
removeE :: [(Peca,Coordenadas)] -> Jogador -> [(Peca,Coordenadas)]
removeE ((h,(a,b)):(i,(c,d)):t) (Jogador (x,y) dir caixa)
    |a==x+1 && b==y = ((i,(c,d)): t)
    |otherwise = (h,(a,b)) : removeE ((i,(c,d)): t) (Jogador (x,y) dir caixa)