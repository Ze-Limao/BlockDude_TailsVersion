module Tarefa2_2021li1g092_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g092
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    , "Tarefa 2 - Teste Desconstruir Mapa ter duas linhas vazias" ~: desconstroiMapa [[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] ~=? [(Bloco,(0,2)),(Bloco,(4,2)),(Bloco,(0,3)),(Porta,(1,3)),(Caixa,(2,3)),(Bloco,(4,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4))]
    ]