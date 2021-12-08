module Tarefa4_2021li1g092_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g092
import Tarefa4_2021li1g092
import Fixtures


testsT4 =
  test
    [ "Tarefa 4 - Teste move m1e1 Oeste e queda" ~: Jogo m1r (Jogador (5,3) Oeste False) ~=?  moveJogador m1e1 AndarEsquerda
    , "Tarefa 4 - Teste move m1e1 Este" ~: Jogo m1r (Jogador (3,3) Este False) ~=?  moveJogador m1e2 AndarDireita
    , "Tarefa 4 - Teste move m1e1 nao ter o que Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    , "Tarefa 4 - Teste moves esquerda" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]    
    , "Tarefa 4 - Teste moves direita " ~: Jogo m1r (Jogador (5,3) Este False) ~=? correrMovimentos m1e2 [AndarDireita, Trepar, AndarDireita]
    , "Tarefa 4 - Teste moves ir contra uma parede ou caixa" ~: Jogo m1r (Jogador (5,3) Oeste False) ~=? correrMovimentos m1e1 [AndarEsquerda,AndarDireita,AndarDireita,AndarEsquerda]
    , "Tarefa 4 - Teste Move m1e1 Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    , "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    ]