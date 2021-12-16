{- |
Module      : Tarefa5_2021li1gXXX
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy (loadJuicy)
import GHC.Float
import Funcoesuteis
import LI12122
import Tarefa2_2021li1g092
import Tarefa4_2021li1g092

type Estado = (Jogo,(Picture,Picture,Picture,Picture,Picture,Picture,Picture)) 

estadoGlossInicial :: Jogo -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Estado
estadoGlossInicial j p1 p2 p3 p4 p5 p6 p7 = (j,(p1,p2,p3,p4,p5,p6,p7))

dm :: Display
dm = FullScreen

reageEventoGloss :: Event -> Estado -> Estado 
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (x,y) = (moveJogador x Trepar ,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  (x,y) = (moveJogador x InterageCaixa ,y)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  (x,y) = (moveJogador x AndarEsquerda ,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (x,y) = (moveJogador x AndarDireita ,y) 
reageEventoGloss _ s = s -- ignora qualquer outro evento 

desenhaEstadoGloss :: Estado -> Picture
desenhaEstadoGloss e@((Jogo mapa (Jogador (x,y) dir caixa)),(p1,p2,p3,p4,p5,p6,p7)) = desenhar  (desconstroiMapa mapa) e

desenhar :: [(Peca,Coordenadas)] -> Estado -> Picture
desenhar l e = ajustar l (Pictures(desenharJ e ++ desenharM e l))

desenharJ :: Estado -> [Picture]
desenharJ ((Jogo mapa (Jogador (x,y) dir False)),(p1,p2,p3,p4,p5,p6,p7)) = 
    if  dir == Este then [Translate (int2Float(x * 60)) (int2Float(-y * 60)) p1]
                    else [Translate (int2Float(x * 60)) (int2Float(-y * 60)) p2]
desenharJ ((Jogo mapa (Jogador (x,y) dir True)),(p1,p2,p3,p4,p5,p6,p7)) = 
    if  dir == Este then [Translate (int2Float(x * 60)) (int2Float ((-y * 60))) p6 ] 
                    else [Translate (int2Float(x * 60)) (int2Float ((-y * 60))) p7 ] 

desenharM :: Estado -> [(Peca,Coordenadas)] -> [Picture]
desenharM e ((h,(a,b)):t) = map (desenharM' e) ((h,(a,b)):t) 

desenharM' :: Estado -> (Peca,Coordenadas) -> Picture
desenharM' ((Jogo mapa (Jogador (x,y) dir caixa)),(p1,p2,p3,p4,p5,p6,p7)) (h,(a,b)) 
  |h == Caixa = Translate (int2Float(a * 60)) (int2Float(-b * 60)) p3
  |h == Porta = Translate (int2Float(a * 60)) (int2Float(-b * 60)) p4
  |otherwise = Translate (int2Float(a * 60)) (int2Float(-b * 60)) p5

ajustar :: [(Peca,Coordenadas)] -> Picture -> Picture
ajustar l pic = Translate (int2Float(-1* ( div (comprimento l * 60) 2))) (int2Float( div (altura l * 60) 2)) pic

main :: IO () 
main = do 
    Just p1 <- loadJuicy "tails_direita.png" 
    Just p2 <- loadJuicy "tails_esquerda.png"
    Just p3 <- loadJuicy "box.png"
    Just p4 <- loadJuicy "ring.png"
    Just p5 <- loadJuicy "bloco.png"
    Just p6 <- loadJuicy "tails_com_caixa_direita.png"  --atualizar imagem
    Just p7 <- loadJuicy "tails_com_caixa_esquerda.png"
    play 
      dm                                                 -- janela onde irá correr o jogo
      azure                                              -- cor do fundo da janela
      60                                                 -- frame rate
      (estadoGlossInicial jogo2 p1 p2 p3 p4 p5 p6 p7)    -- estado inicial
      desenhaEstadoGloss                                 -- desenha o estado do jogo
      reageEventoGloss                                   -- reage a um evento
      (\ _ e -> e)                                       -- reage ao passar do tempo


jogo1 = (Jogo [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Caixa, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (5,3) Oeste False))

jogo2 = (Jogo [[Vazio,Bloco],[Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco],[Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco],[Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco]](Jogador (2,5) Oeste False))