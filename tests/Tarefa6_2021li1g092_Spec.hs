module Tarefa6_2021li1g092_Spec where

import Test.HUnit
import Tarefa6_2021li1g092
import LI12122
import Fixtures

testsT6 =
  test
    [ "Tarefa 6 - Teste resolver m1e1 que tem um buraco para o vazio" ~: Just [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=?  resolveJogo 6 m1e1
     , "Tarefa 6 - Teste resolver myLevel1 que requer interação com caixas " ~: Just [InterageCaixa,AndarEsquerda,AndarEsquerda,Trepar,InterageCaixa,Trepar,Trepar] ~=?  resolveJogo 7 (Jogo [[Bloco,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Bloco,Bloco, Caixa, Caixa, Vazio, Caixa, Vazio, Bloco],[Bloco,Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (6,4) Oeste False))
    ]   