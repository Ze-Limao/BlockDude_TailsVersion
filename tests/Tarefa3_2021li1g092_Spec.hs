module Tarefa3_2021li1g092_Spec where

import Test.HUnit
import Tarefa3_2021li1g092
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    ]