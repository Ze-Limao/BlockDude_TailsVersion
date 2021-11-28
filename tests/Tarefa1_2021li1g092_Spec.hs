module Tarefa1_2021li1g092_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g092
import Fixtures
import Data.Bool (Bool(False))

testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa quando há caixas a flutuar" ~:  False ~=? validaPotencialMapa [(Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4,2)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: False ~=? validaPotencialMapa []
    , "Tarefa 1 - Teste Valida Mapa com duas peças na mesma coordenada" ~: False ~=? validaPotencialMapa [(Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4,2)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3))]
    , "Tarefa 1 - Teste Valida Mapa com pelo menos uma linha vazia" ~: True ~=? validaPotencialMapa [(Bloco,(1,4)),(Bloco,(5,2)),(Bloco,(0,0)),(Bloco,(1,5)),(Bloco,(0,5)),(Bloco,(5,0)),(Bloco,(5,5)),(Bloco,(2,5)),(Caixa,(3,4)),(Bloco,(1,0)),(Bloco,(4,0)),(Porta,(1,3)),(Bloco,(2,0)),(Bloco,(5,3)),(Bloco,(3,0)),(Caixa,(0,4)),(Bloco,(3,5)),(Bloco,(0,4)),(Bloco,(5,4)),(Bloco,(0,3)),(Bloco,(4,5))]
    , "Tarefa 1 - Teste Valida Mapa com mais que uma porta" ~: False ~=? validaPotencialMapa [(Bloco,(0,4)),(Porta, (1,4)),(Bloco,(7,0)),(Bloco,(6,4)),(Bloco,(1,3)),(Caixa,(2,1)),(Bloco,(2,4)),(Bloco,(5,4)),(Bloco,(3,4)),(Bloco,(1,5)),(Porta,(1,2)),(Bloco,(4,5)),(Bloco,(2,2)),(Bloco,(7,4)),(Bloco,(7,1)),(Caixa,(2,3)),(Bloco,(4,4)),(Porta,(3,3)),(Bloco,(7,1)),(Bloco,(7,3)),(Caixa,(5,3)),(Bloco,(7,2))]
    , "Tarefa 1 - Teste Valida Mapa sem espaços livres"~: False ~=? validaPotencialMapa [(Bloco,(0,2)),(Caixa,(2,1)),(Bloco,(0,1)),(Bloco,(2,0)),(Bloco,(2,2)),(Porta,(1,0)),(Bloco,(1,2)),(Caixa,(0,0)),(Bloco,(1,1))]
    , "Tarefa 1 - Teste Valida Mapa com buracos"~: False ~=? validaPotencialMapa [(Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]
    ]
