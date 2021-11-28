module Tarefa3_2021li1g092_Spec where

import Test.HUnit
import Tarefa3_2021li1g092
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo Jogador Este" ~: "       \n      X\n      X\nP > C X\nXXXXXXX" ~=?  show m1e3
    , "Tarefa 3 - Teste Imprime Jogo onde o Jogador sem caixa nao tem espaco para nascer" ~: "" ~=? show m1e4
    , "Tarefa 3 - Teste Imprime Jogo em que o jogador apenas nao tem espaco se estiver com a caixa " ~: "" ~=? show m1e5
    ]
    