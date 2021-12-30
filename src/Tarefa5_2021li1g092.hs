{- |
Module      : Tarefa5_2021li1gXXX
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game 
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy (loadJuicy)
import GHC.Float
import Funcoesuteis
import LI12122
import Tarefa2_2021li1g092
import Tarefa4_2021li1g092
import System.Exit
import System.Process

data Menu = MainMenu OptionsM | PlayMenu OptionsP
data Figura = Rectangule Float Float Color 
data OptionsM = Inicial | Jogar  |  Sair
data OptionsP = Nivel1 | Nivel2 | Nivel3 | Finale

type Estado = (Menu,Jogo,(Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture)) 

--------------------------------------------------------    AUX   -------------------------------------------------------------------

audio1 :: IO (ProcessHandle) 
audio1 = spawnCommand "mpv MenuTheme.mp3"

audio2 :: IO (ProcessHandle) 
audio2 = spawnCommand "mpv GreenHill.mp3"

audio3 :: IO (ProcessHandle) 
audio3 = spawnCommand "mpv FinalLevel.mp3"

audio4 :: IO (ProcessHandle) 
audio4 = spawnCommand "mpv End.mp3"

stop :: IO (ProcessHandle) 
stop = spawnCommand "killall mpv"

 
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


estadoGlossInicial :: Menu -> Jogo -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Estado
estadoGlossInicial m j p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 = (m,j,(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12))


reageEventoGloss :: Event -> Estado -> IO Estado 
reageEventoGloss _ ((MainMenu Inicial),x,y) = do audio1
                                                 return ((MainMenu Jogar),x,y)
reageEventoGloss (EventKey (SpecialKey KeyEsc) Down _ _) (_,x,y) = do stop
                                                                      exitSuccess
--Menu
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((MainMenu Jogar),x,y) = return ((MainMenu Jogar),x,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((MainMenu Jogar),x,y) = return ((MainMenu Sair),x,y)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((MainMenu Sair),x,y) = return ((MainMenu Jogar),x,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((MainMenu Sair),x,y) = return ((MainMenu Sair),x,y) 
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) ((MainMenu Jogar),x,y) = do stop
                                                                                       audio2 
                                                                                       return ((PlayMenu Nivel1), jogo1, y)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) ((MainMenu Sair),x,y) = do stop
                                                                                      exitSuccess
reageEventoGloss _ ((MainMenu Jogar),x,y) = return ((MainMenu Jogar),x,y) 
reageEventoGloss _ ((MainMenu Sair),x,y) = return ((MainMenu Sair),x,y) 
--Play
--Level1
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    ((PlayMenu Nivel1),x,y) = return ((PlayMenu Nivel1) ,moveJogador x Trepar ,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  ((PlayMenu Nivel1),x,y) = return ((PlayMenu Nivel1) ,moveJogador x InterageCaixa ,y)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  ((PlayMenu Nivel1),x,y) = return ((PlayMenu Nivel1) ,moveJogador x AndarEsquerda ,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((PlayMenu Nivel1),x,y) = return ((PlayMenu Nivel1) ,moveJogador x AndarDireita ,y) 
reageEventoGloss (EventKey (SpecialKey KeyF1) Down _ _) ((PlayMenu Nivel1),x,y) = return ((PlayMenu Nivel1) ,jogo1 , y) --Restart
reageEventoGloss _ ((PlayMenu Nivel1),x,y) = 
    if porta1 x == True then return ((PlayMenu Nivel2),jogo2,y) 
                        else return ((PlayMenu Nivel1,x,y))
--Level2
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    ((PlayMenu Nivel2),x,y) = return ((PlayMenu Nivel2) ,moveJogador x Trepar ,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  ((PlayMenu Nivel2),x,y) = return ((PlayMenu Nivel2) ,moveJogador x InterageCaixa ,y)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  ((PlayMenu Nivel2),x,y) = return ((PlayMenu Nivel2) ,moveJogador x AndarEsquerda ,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((PlayMenu Nivel2),x,y) = return ((PlayMenu Nivel2) ,moveJogador x AndarDireita ,y) 
reageEventoGloss (EventKey (SpecialKey KeyF1) Down _ _) ((PlayMenu Nivel2),x,y) = return ((PlayMenu Nivel2) ,jogo2 , y) --Restart
reageEventoGloss _ ((PlayMenu Nivel2),x,y) = 
    if porta2 x == True then do stop
                                audio3 
                                return ((PlayMenu Nivel3),jogo3,y)
                        else return ((PlayMenu Nivel2,x,y))
--Level3
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)  ((PlayMenu Nivel3),x,y) = return ((PlayMenu Nivel3) ,moveJogador x Trepar ,y)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  ((PlayMenu Nivel3),x,y) = return ((PlayMenu Nivel3) ,moveJogador x InterageCaixa ,y)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  ((PlayMenu Nivel3),x,y) = return ((PlayMenu Nivel3) ,moveJogador x AndarEsquerda ,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((PlayMenu Nivel3),x,y) = return ((PlayMenu Nivel3) ,moveJogador x AndarDireita ,y) 
reageEventoGloss (EventKey (SpecialKey KeyF1) Down _ _) ((PlayMenu Nivel3),x,y) = return ((PlayMenu Nivel3) ,jogo3 , y) --Restart
reageEventoGloss _ ((PlayMenu Nivel3),x,y) = 
    if porta3 x == True then do stop
                                audio4
                                return ((PlayMenu Finale),jogo3,y) 
                        else return ((PlayMenu Nivel3,x,y))  
--Finale                        
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) ((PlayMenu Finale),x,y) = do stop
                                                                                        audio1
                                                                                        return ((MainMenu Jogar),x,y)
reageEventoGloss _ ((PlayMenu Finale),x,y) = return ((PlayMenu Finale),x,y)


desenhaEstadoGloss :: Estado -> IO Picture
desenhaEstadoGloss ((PlayMenu Finale),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = return p12
desenhaEstadoGloss e@((PlayMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = return $ desenharPlay  (desconstroiMapa mapa) e
desenhaEstadoGloss f@((MainMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = return $ desenharMenu f

desenharMenu :: Estado -> Picture
desenharMenu ((MainMenu Jogar),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = 
    Pictures ( [scale 1.01 1.01 p0] ++ [Translate (-245) (-245) (scale 0.6 0.6 p9) ] )
desenharMenu ((MainMenu Sair),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = 
    Pictures ( [scale 1.01 1.01 p0] ++ [Translate (-195) (-375) (scale 0.6 0.6 p9) ] )

desenharPlay :: [(Peca,Coordenadas)] -> Estado -> Picture
desenharPlay l e = Pictures ([desenharF e] ++ [ajustar l (Pictures(desenharJ e ++ desenharM e l))])

desenharF :: Estado -> Picture
desenharF ((PlayMenu Finale),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = p12
desenharF ((PlayMenu Nivel3),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = scale 1.5 1.5 (Translate 0 50 p11)
desenharF ((PlayMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = scale 1.1 1.1 p8 

desenharJ :: Estado -> [Picture]
desenharJ ((PlayMenu _),(Jogo mapa (Jogador (x,y) dir False)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = 
    if  dir == Este then [Translate (int2Float(x * 60)) (int2Float(-y * 60)) p1]
                    else [Translate (int2Float(x * 60)) (int2Float(-y * 60)) p2]
desenharJ ((PlayMenu _),(Jogo mapa (Jogador (x,y) dir True)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) = 
    if  dir == Este then [Translate (int2Float(x * 60)) (int2Float ((-y * 60))) p6] 
                    else [Translate (int2Float(x * 60)) (int2Float ((-y * 60))) p7] 

desenharM :: Estado -> [(Peca,Coordenadas)] -> [Picture]
desenharM e ((h,(a,b)):t) = (desenharM' e) `map` ((h,(a,b)):t) 

desenharM' :: Estado -> (Peca,Coordenadas) -> Picture
desenharM' ((PlayMenu _),(Jogo mapa (Jogador (x,y) dir caixa)),(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)) (h,(a,b)) 
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
    Just p8  <- loadJuicy "fundo1.png" 
    Just p9  <- loadJuicy "cursor.png" 
    Just p10 <- loadJuicy "box2.png"
    Just p11 <- loadJuicy "fundo2.png"
    Just p12 <- loadJuicy "finale.png"
    playIO 
      (FullScreen)                                                  
      (makeColorI 52 102 255 0)                                         
      (60)                                                          
      (estadoGlossInicial (MainMenu Inicial) jogo1 p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12)    
      (desenhaEstadoGloss)                                        
      (reageEventoGloss)                                          
      (\ _ e -> return e)                                                


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------