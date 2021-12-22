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
import System.Exit

data Menu = MainMenu OptionsM | PlayMenu OptionsP
data Figura = Rectangule Float Float Color 
data OptionsM = Jogar | Sair
data OptionsP = Nivel1 | Nivel2 | Nivel3 | Finale

type Estado = (Menu,Jogo,(Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture)) 

--------------------------------------------------------    AUX   -------------------------------------------------------------------


figuraToPicture :: Figura -> Picture
figuraToPicture (Rectangule l a c) = Color c $ rectangleSolid l a
{-} 
opcaoJogar = figuraToPicture (Rectangule 300  120 (greyN 0.8))
opcaoSair = figuraToPicture (Rectangule 300  120 (greyN 0.8))
selctJogar = figuraToPicture (Rectangule 310  130 (greyN 0.4))       Coisas antigas
selctSair = figuraToPicture (Rectangule 310  130 (greyN 0.4)) 
draw (Jogar) = scale 0.7 0.7 $ Pictures [ (Translate (-160) (-200)) $ Color black $ Text "Jogar" ] 
draw (Sair) = scale 0.7 0.7 $ Pictures [ (Translate (-120) (-420)) $ Color black $ Text "Sair" ] 
-}
 
jogo1 = (Jogo [[Bloco,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Caixa, Caixa, Vazio, Caixa, Vazio, Bloco],[Bloco,Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (6,4) Oeste False))

jogo2 = (Jogo [[Vazio,Bloco],[Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco],[Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco],[Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco]](Jogador (5,7) Este False))

jogo3 = (Jogo [[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Bloco,Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco],[Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio],[Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Caixa,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],[Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Caixa,Caixa,Caixa,Vazio,Vazio,Bloco,Bloco,Vazio,Caixa,Caixa,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]] (Jogador (17,7) Oeste False))

porta1 :: Jogo ->  Bool
porta1 ((Jogo mapa (Jogador (x,y) dir caixa))) = if x==1 && y==1 then True else False

porta2 :: Jogo ->  Bool
porta2 ((Jogo mapa (Jogador (x,y) dir caixa))) = if x==1 && y==9 then True else False

porta3 :: Jogo ->  Bool
porta3 ((Jogo mapa (Jogador (x,y) dir caixa))) = if x==1 && y==5 then True else False

-----------------------------------------------------   FUNÇÃO MAIN   ------------------------------------------------------------------------------------------------------------------------------------------------------------


estadoGlossInicial :: Menu -> Jogo -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Estado
estadoGlossInicial m j p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 = (m,j,(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11))


reageEventoGloss :: Event -> Estado -> Estado 
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    ((PlayMenu Nivel1),x,y) = ((PlayMenu Nivel1) ,moveJogador x Trepar ,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  ((PlayMenu Nivel1),x,y) = ((PlayMenu Nivel1) ,moveJogador x InterageCaixa ,y)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  ((PlayMenu Nivel1),x,y) = ((PlayMenu Nivel1) ,moveJogador x AndarEsquerda ,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((PlayMenu Nivel1),x,y) = ((PlayMenu Nivel1) ,moveJogador x AndarDireita ,y) 
reageEventoGloss (EventKey (SpecialKey KeyF2) Down _ _) ((PlayMenu Nivel1),x,y) = ((PlayMenu Nivel1) ,jogo1 , y) --Restart
reageEventoGloss _ ((PlayMenu Nivel1),x,y) = if porta1 x == True then ((PlayMenu Nivel2),jogo2,y) else ((PlayMenu Nivel1,x,y))
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    ((PlayMenu Nivel2),x,y) = ((PlayMenu Nivel2) ,moveJogador x Trepar ,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  ((PlayMenu Nivel2),x,y) = ((PlayMenu Nivel2) ,moveJogador x InterageCaixa ,y)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  ((PlayMenu Nivel2),x,y) = ((PlayMenu Nivel2) ,moveJogador x AndarEsquerda ,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((PlayMenu Nivel2),x,y) = ((PlayMenu Nivel2) ,moveJogador x AndarDireita ,y) 
reageEventoGloss (EventKey (SpecialKey KeyF2) Down _ _) ((PlayMenu Nivel2),x,y) = ((PlayMenu Nivel2) ,jogo2 , y) --Restart
reageEventoGloss _ ((PlayMenu Nivel2),x,y) = if porta2 x == True then ((PlayMenu Nivel3),jogo3,y) else ((PlayMenu Nivel2,x,y))
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)  ((PlayMenu Nivel3),x,y) = ((PlayMenu Nivel3) ,moveJogador x Trepar ,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  ((PlayMenu Nivel3),x,y) = ((PlayMenu Nivel3) ,moveJogador x InterageCaixa ,y)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  ((PlayMenu Nivel3),x,y) = ((PlayMenu Nivel3) ,moveJogador x AndarEsquerda ,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((PlayMenu Nivel3),x,y) = ((PlayMenu Nivel3) ,moveJogador x AndarDireita ,y) 
reageEventoGloss (EventKey (SpecialKey KeyF2) Down _ _) ((PlayMenu Nivel3),x,y) = ((PlayMenu Nivel3) ,jogo3 , y) --Restart
reageEventoGloss _ ((PlayMenu Nivel3),x,y) = if porta3 x == True then ((PlayMenu Nivel3 ),jogo3,y) else ((PlayMenu Nivel3,x,y))  --- FINALEEE
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((MainMenu Jogar),x,y) = ((MainMenu Jogar),x,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((MainMenu Jogar),x,y) = ((MainMenu Sair),x,y)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((MainMenu Sair),x,y) = ((MainMenu Jogar),x,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((MainMenu Sair),x,y) = ((MainMenu Sair),x,y) 
reageEventoGloss (EventKey (SpecialKey KeyF1) Down _ _) ((MainMenu Jogar),x,y) = ((PlayMenu Nivel1), jogo1, y)
--reageEventoGloss (EventKey (SpecialKey KeyF1) Down _ _) ((MainMenu Sair),x,y) = ExitSuccess
reageEventoGloss _ ((MainMenu Jogar),x,y) = ((MainMenu Jogar),x,y) 
reageEventoGloss _ ((MainMenu Sair),x,y) = ((MainMenu Sair),x,y) 


desenhaEstadoGloss :: Estado -> Picture
desenhaEstadoGloss e@((PlayMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) = desenharPlay  (desconstroiMapa mapa) e
desenhaEstadoGloss f@((MainMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) = desenharMenu f

desenharMenu :: Estado -> Picture
desenharMenu ((MainMenu Jogar),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) = 
    Pictures ( [scale 1.01 1.01 p0] ++ [Translate (-245) (-245) (scale 0.6 0.6 p9) ] )
desenharMenu ((MainMenu Sair),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) = 
    Pictures ( [scale 1.01 1.01 p0] ++ [Translate (-195) (-375) (scale 0.6 0.6 p9) ] )

desenharPlay :: [(Peca,Coordenadas)] -> Estado -> Picture
desenharPlay l e = Pictures ([desenharF e] ++ [ajustar l (Pictures(desenharJ e ++ desenharM e l))])

desenharF :: Estado -> Picture
desenharF ((PlayMenu Nivel3),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) = scale 1.2 1.2 (Translate 0 50 p11)
desenharF ((PlayMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) = scale 2.5 2.5 (Translate 0 (-50) p8) 

desenharJ :: Estado -> [Picture]
desenharJ ((PlayMenu _),(Jogo mapa (Jogador (x,y) dir False)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) = 
    if  dir == Este then [Translate (int2Float(x * 60)) (int2Float(-y * 60)) p1]
                    else [Translate (int2Float(x * 60)) (int2Float(-y * 60)) p2]
desenharJ ((PlayMenu _),(Jogo mapa (Jogador (x,y) dir True)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) = 
    if  dir == Este then [Translate (int2Float(x * 60)) (int2Float ((-y * 60))) p6] 
                    else [Translate (int2Float(x * 60)) (int2Float ((-y * 60))) p7] 

desenharM :: Estado -> [(Peca,Coordenadas)] -> [Picture]
desenharM e ((h,(a,b)):t) = (desenharM' e) `map` ((h,(a,b)):t) 

desenharM' :: Estado -> (Peca,Coordenadas) -> Picture
desenharM' ((PlayMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)) (h,(a,b)) 
  |h == Caixa = if dir == Oeste then Translate (int2Float(a * 60)) (int2Float(-b * 60)) p10 else Translate (int2Float(a * 60)) (int2Float(-b * 60)) p3
  |h == Porta = Translate (int2Float(a * 60)) (int2Float(-b * 60)) p4
  |otherwise = Translate (int2Float(a * 60)) (int2Float(-b * 60)) p5

ajustar :: [(Peca,Coordenadas)] -> Picture -> Picture
ajustar l pic = Translate (int2Float(-1* ( div (comprimento l * 60) 2))) (int2Float( div (altura l * 60) 2)) pic


-----------------------------------------------------   PLAY   --------------------------------------------------------------------------------------------------------------------------------


main :: IO () 
main = do 
    Just p0  <- loadJuicy "mainmenu.png" 
    Just p1  <- loadJuicy "tails_direita.png" 
    Just p2  <- loadJuicy "tails_esquerda.png"
    Just p3  <- loadJuicy "box.png"
    Just p4  <- loadJuicy "ring.png"
    Just p5  <- loadJuicy "bloco.png"
    Just p6  <- loadJuicy "tails_com_caixa_direita.png" 
    Just p7  <- loadJuicy "tails_com_caixa_esquerda.png"
    Just p8  <- loadJuicy "fundo.png" 
    Just p9  <- loadJuicy "cursor.png" 
    Just p10 <- loadJuicy "box2.png"
    Just p11 <- loadJuicy "cat.png"
    play 
      (FullScreen)                                                  
      (makeColorI 52 102 255 0)                                         
      (60)                                                          
      (estadoGlossInicial (MainMenu Jogar) jogo1 p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11)    
      (desenhaEstadoGloss)                                        
      (reageEventoGloss)                                          
      (\ _ e -> e)                                                


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------