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
import Mapas
import Funcoesuteis
import LI12122
import Tarefa2_2021li1g092
import Tarefa4_2021li1g092
import System.Exit

data Menu = MainMenu Options | PlayMenu 
data Figura = Rectangule Float Float Color 
data Options = Jogar | Sair

type Estado = (Menu,Jogo,(Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture)) 

--------------------------------------------------------    AUX   -------------------------------------------------------------------


figuraToPicture :: Figura -> Picture
figuraToPicture (Rectangule l a c) = Color c $ rectangleSolid l a

opcaoJogar = figuraToPicture (Rectangule 300  120 (greyN 0.8))
opcaoSair = figuraToPicture (Rectangule 300  120 (greyN 0.8))
selctJogar = figuraToPicture (Rectangule 310  130 (greyN 0.4))
selctSair = figuraToPicture (Rectangule 310  130 (greyN 0.4)) 

draw (Jogar) = scale 0.7 0.7 $ Pictures [ (Translate (-160) (-200)) $ Color black $ Text "Jogar" ] 
draw (Sair) = scale 0.7 0.7 $ Pictures [ (Translate (-120) (-420)) $ Color black $ Text "Sair" ] 


-----------------------------------------------------   FUNÇÃO MAIN   ---------------------------------------------------------------------------


estadoGlossInicial :: Menu -> Jogo -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Estado
estadoGlossInicial m j p0 p1 p2 p3 p4 p5 p6 p7 p8 = (m,j,(p0,p1,p2,p3,p4,p5,p6,p7,p8))


reageEventoGloss :: Event -> Estado -> Estado 
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    ((PlayMenu),x,y) = ((PlayMenu) ,moveJogador x Trepar ,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  ((PlayMenu),x,y) = ((PlayMenu) ,moveJogador x InterageCaixa ,y)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  ((PlayMenu),x,y) = ((PlayMenu) ,moveJogador x AndarEsquerda ,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((PlayMenu),x,y) = ((PlayMenu) ,moveJogador x AndarDireita ,y) 
reageEventoGloss (EventKey (SpecialKey KeyF2) Down _ _) ((PlayMenu),x,y) = ((PlayMenu) ,jogo1 , y) --Restart
reageEventoGloss _ ((PlayMenu),x,y) = ((PlayMenu),x,y) 
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((MainMenu Jogar),x,y) = ((MainMenu Jogar),x,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((MainMenu Jogar),x,y) = ((MainMenu Sair),x,y)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((MainMenu Sair),x,y) = ((MainMenu Jogar),x,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((MainMenu Sair),x,y) = ((MainMenu Sair),x,y) 
reageEventoGloss (EventKey (SpecialKey KeyF1) Down _ _) ((MainMenu Jogar),x,y) = ((PlayMenu), jogo1, y)
--reageEventoGloss (EventKey (SpecialKey KeyF1) Down _ _) ((MainMenu Sair),x,y) = ((MainMenu Sair),x,y)  --exitSuccess
reageEventoGloss _ ((MainMenu Jogar),x,y) = ((MainMenu Jogar),x,y) 
reageEventoGloss _ ((MainMenu Sair),x,y) = ((MainMenu Sair),x,y) 


desenhaEstadoGloss :: Estado -> Picture
desenhaEstadoGloss e@((PlayMenu),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8)) = desenharPlay  (desconstroiMapa mapa) e
desenhaEstadoGloss e@((MainMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8)) = Pictures ( [desenharMenu e] ++ [p0])

desenharMenu :: Estado -> Picture
desenharMenu ((MainMenu Jogar),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8)) = 
    Pictures ( [Translate 0 (-120) selctJogar] ++ [Translate 0 (-120) opcaoJogar] ++ [Translate 0 (-260) opcaoSair] ++ [draw (Jogar)] ++ [draw (Sair)] )
desenharMenu ((MainMenu Sair),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8)) = 
    Pictures ( [Translate 0 (-260) selctSair] ++ [Translate 0 (-120) opcaoJogar] ++ [Translate 0 (-260) opcaoSair] ++ [draw (Jogar)] ++ [draw (Sair)] )

desenharPlay :: [(Peca,Coordenadas)] -> Estado -> Picture
desenharPlay l e = Pictures ([desenharF e] ++ [ajustar l (Pictures(desenharJ e ++ desenharM e l))])

desenharF :: Estado -> Picture
desenharF ((PlayMenu),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8)) = scale 2.5 2.5 (Translate 0 (-50) p8) 

desenharJ :: Estado -> [Picture]
desenharJ ((PlayMenu),(Jogo mapa (Jogador (x,y) dir False)),(p0,p1,p2,p3,p4,p5,p6,p7,p8)) = 
    if  dir == Este then [Translate (int2Float(x * 60)) (int2Float(-y * 60)) p1]
                    else [Translate (int2Float(x * 60)) (int2Float(-y * 60)) p2]
desenharJ ((PlayMenu),(Jogo mapa (Jogador (x,y) dir True)),(p0,p1,p2,p3,p4,p5,p6,p7,p8)) = 
    if  dir == Este then [Translate (int2Float(x * 60)) (int2Float ((-y * 60))) p6] 
                    else [Translate (int2Float(x * 60)) (int2Float ((-y * 60))) p7] 

desenharM :: Estado -> [(Peca,Coordenadas)] -> [Picture]
desenharM e ((h,(a,b)):t) = (desenharM' e) `map` ((h,(a,b)):t) 

desenharM' :: Estado -> (Peca,Coordenadas) -> Picture
desenharM' ((PlayMenu),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8)) (h,(a,b)) 
  |h == Caixa = Translate (int2Float(a * 60)) (int2Float(-b * 60)) p3
  |h == Porta = Translate (int2Float(a * 60)) (int2Float(-b * 60)) p4
  |otherwise = Translate (int2Float(a * 60)) (int2Float(-b * 60)) p5

ajustar :: [(Peca,Coordenadas)] -> Picture -> Picture
ajustar l pic = Translate (int2Float(-1* ( div (comprimento l * 60) 2))) (int2Float( div (altura l * 60) 2)) pic


-----------------------------------------------------   PLAY   ---------------------------------------------------------------------------


main :: IO () 
main = do 
    Just p0 <- loadJuicy "fundomenu.png" 
    Just p1 <- loadJuicy "tails_direita.png" 
    Just p2 <- loadJuicy "tails_esquerda.png"
    Just p3 <- loadJuicy "box.png"
    Just p4 <- loadJuicy "ring.png"
    Just p5 <- loadJuicy "bloco.png"
    Just p6 <- loadJuicy "tails_com_caixa_direita.png" 
    Just p7 <- loadJuicy "tails_com_caixa_esquerda.png"
    Just p8 <- loadJuicy "fundo.png" 
    play 
      (FullScreen)                                                  
      (makeColorI 29 31 47 0)                                         
      (60)                                                          
      (estadoGlossInicial (MainMenu Jogar) jogo1 p0 p1 p2 p3 p4 p5 p6 p7 p8)    
      (desenhaEstadoGloss)                                        
      (reageEventoGloss)                                          
      (\ _ e -> e)                                                


------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------