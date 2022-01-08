module Tarefa2_2021li1g092_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g092
import Fixtures


testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa vazio" ~: [[]] ~=? constroiMapa []
    , "Tarefa 2 - Teste Construir Mapa " ~: m1r ~=? constroiMapa m1
    ,  "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa [[]]
    , "Tarefa 2 - Teste Desconstruir Mapa " ~: [(Bloco,(0,2)),(Bloco,(4,2)),(Bloco,(0,3)),(Porta,(1,3)),(Caixa,(2,3)),(Bloco,(4,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4))] ~=? desconstroiMapa [[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]
    ]