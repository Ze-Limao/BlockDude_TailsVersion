import Control.Monad
import Control.Exception

-- import Data.CallStack
import Data.List (sort)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout
import qualified System.Exit as Exit

import LI12122

import Tarefa1_2021li1g092
import Tarefa2_2021li1g092
import Tarefa3_2021li1g092
import Tarefa4_2021li1g092

import Test.HUnit hiding ((~=?))
import qualified Test.HUnit as T ((~=?))

(~=?) expected actual =
  unsafePerformIO
    (do x <-
          timeout timeoutVal $
          catch
            (return $! actual)
            (\e -> return $ error $ displayException (e :: SomeException))
        y <-
          timeout timeoutVal $
          catch
            (return $! expected)
            (\e -> return $ error $ displayException (e :: SomeException))
        case (x, y) of
          (Just e, Just a) -> return $ (T.~=?) e a
          _ ->
            return $ (T.~=?) ((error "timeout") :: ()) ((error "timeout") :: ()))
  where
    timeoutVal = 1000000 * nsec
    nsec = 1 -- timeout for nsec seconds

insertAt :: (Int, Int) -> a -> [[a]] -> [[a]]
insertAt (x, y) item grelha = take y grelha ++ linha : drop (y + 1) grelha
  where
    lista = grelha !! y
    linha = take x lista ++ item : drop (x + 1) lista

invalido01 :: [(Peca, Coordenadas)]
invalido01 = [ (Bloco, (4, 5)), (Bloco, (12, 5)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Caixa, (15, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (14, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

invalido02 :: [(Peca, Coordenadas)]
invalido02 = [ (Bloco, (4, 5)), (Bloco, (12, 5)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

invalido03 :: [(Peca, Coordenadas)]
invalido03 = [ (Bloco, (0, 0)), (Bloco, (19, 0)), (Bloco, (0, 1)), (Bloco, (19, 1)), (Bloco, (0, 2)), (Bloco, (19, 2)), (Bloco, (0, 3)), (Bloco, (19, 3)), (Bloco, (0, 4)), (Bloco, (19, 4)), (Bloco, (0, 5)), (Bloco, (4, 5)), (Caixa, (12, 4)), (Bloco, (19, 5)), (Bloco, (0, 6)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Caixa, (15, 6)), (Bloco, (19, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (14, 7)), (Bloco, (15, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

invalido04 :: [(Peca, Coordenadas)]
invalido04 = [ (Bloco, (19, 0)), (Bloco, (18, 1)), (Bloco, (20, 1)), (Bloco, (17, 2)), (Bloco, (21, 2)), (Bloco, (7, 3)), (Bloco, (16, 3)), (Bloco, (22, 3)), (Bloco, (6, 4)), (Bloco, (8, 4)), (Bloco, (15, 4)), (Bloco, (23, 4)), (Bloco, (3, 5)), (Bloco, (4, 5)), (Bloco, (5, 5)), (Bloco, (9, 5)), (Bloco, (14, 5)), (Bloco, (24, 5)), (Bloco, (2, 6)), (Bloco, (10, 6)), (Bloco, (13, 6)), (Bloco, (24, 6)), (Bloco, (1, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (24, 7)), (Bloco, (1, 8)), (Bloco, (24, 8)), (Bloco, (1, 9)), (Caixa, (23, 9)), (Bloco, (24, 9)), (Bloco, (1, 10)), (Caixa, (22, 10)), (Caixa, (23, 10)), (Bloco, (24, 10)), (Bloco, (1, 11)), (Bloco, (22, 11)), (Bloco, (23, 11)), (Bloco, (24, 11)), (Bloco, (0, 12)), (Bloco, (1, 12)), (Bloco, (6, 12)), (Bloco, (17, 12)), (Bloco, (0, 13)), (Porta, (1, 13)), (Bloco, (6, 13)), (Caixa, (8, 13)), (Bloco, (17, 13)), (Bloco, (18, 13)), (Bloco, (19, 13)), (Bloco, (20, 13)), (Bloco, (21, 13)), (Bloco, (22, 13)), (Bloco, (0, 14)), (Bloco, (1, 14)), (Bloco, (2, 14)), (Bloco, (3, 14)), (Bloco, (4, 14)), (Bloco, (6, 14)), (Caixa, (8, 14)), (Caixa, (12, 14)), (Bloco, (15, 14)), (Bloco, (16, 14)), (Bloco, (17, 14)), (Bloco, (4, 15)), (Bloco, (6, 15)), (Caixa, (8, 15)), (Bloco, (10, 15)), (Bloco, (12, 15)), (Caixa, (13, 15)), (Bloco, (15, 15)), (Bloco, (4, 16)), (Bloco, (6, 16)), (Bloco, (7, 16)), (Bloco, (8, 16)), (Bloco, (9, 16)), (Bloco, (10, 16)), (Bloco, (11, 16)), (Bloco, (12, 16)), (Bloco, (13, 16)), (Bloco, (14, 16)), (Bloco, (15, 16)), (Bloco, (4, 17)), (Bloco, (5, 17)), (Bloco, (6, 17)) ]

invalido05 :: [(Peca, Coordenadas)]
invalido05 = [ (Porta, (0, 0)) , (Bloco, (0, 1)) , (Bloco, (1, 1)) , (Porta, (4, 0)) , (Bloco, (2, 1)) , (Bloco, (3, 1)) , (Bloco, (4, 1)) , (Bloco, (5, 1)) ]

invalido06 :: [(Peca, Coordenadas)]
invalido06 = [(Bloco, (0, 1)), (Bloco, (1, 1)), (Bloco, (2, 1)), (Bloco, (3, 1)), (Bloco, (4, 1)), (Bloco, (5, 1))]

invalido07 :: [(Peca, Coordenadas)]
invalido07 = [(Bloco, (5, 0)), (Bloco, (4, 0)), (Bloco, (3, 0)), (Bloco, (2, 0)), (Porta, (1, 0)), (Bloco, (0, 0)), (Bloco, (0, 1)), (Bloco, (1, 1)), (Bloco, (2, 1)), (Bloco, (3, 1)), (Bloco, (4, 1)), (Bloco, (5, 1))]

invalido08 :: [(Peca, Coordenadas)]
invalido08 = [(Bloco, (6, 1)), (Bloco, (2, 2)), (Bloco, (6, 2)), (Porta, (0, 3)), (Bloco, (2, 3)), (Bloco, (6, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4))]

lista01 :: [(Peca, Coordenadas)]
lista01 = [(Porta, (0, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (2, 3)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Caixa, (4, 3)), (Bloco, (5, 4)), (Bloco, (6, 4)), (Bloco, (6, 3)), (Bloco, (6, 2)), (Bloco, (6, 1)) ]

mapa01 :: Mapa
mapa01 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista02 :: [(Peca, Coordenadas)]
lista02 = [ (Porta, (0, 0)), (Bloco, (0, 1)), (Bloco, (1, 1)), (Bloco, (2, 1)), (Bloco, (3, 1)), (Bloco, (4, 1)), (Bloco, (5, 1)) ]

mapa02 :: Mapa
mapa02 =
  [ [Porta, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista03 :: [(Peca, Coordenadas)]
lista03 = [ (Porta, (0, 1)), (Bloco, (2, 1)), (Bloco, (0, 2)), (Bloco, (1, 2)), (Bloco, (2, 2)), (Bloco, (3, 2)), (Bloco, (4, 2)), (Bloco, (5, 2)) ]

mapa03 :: Mapa
mapa03 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista04 :: [(Peca, Coordenadas)]
lista04 = [ (Bloco, (6, 1)), (Bloco, (2, 2)), (Bloco, (6, 2)), (Porta, (0, 3)), (Bloco, (2, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4)) ]

mapa04 :: Mapa
mapa04 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista05 :: [(Peca, Coordenadas)]
lista05 = [ (Bloco, (2, 1)), (Bloco, (6, 1)), (Bloco, (6, 2)), (Porta, (0, 3)), (Bloco, (2, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4)) ]

mapa05 :: Mapa
mapa05 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista06 :: [(Peca, Coordenadas)]
lista06 = [ (Bloco, (6, 1)), (Bloco, (2, 2)), (Bloco, (6, 2)), (Porta, (0, 3)), (Bloco, (2, 3)), (Caixa, (3, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4)) ]

mapa06 :: Mapa
mapa06 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Caixa, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista07 :: [(Peca, Coordenadas)]
lista07 = [ (Bloco, (3, 0)), (Porta, (0, 1)), (Caixa, (2, 1)), (Bloco, (0, 2)), (Bloco, (1, 2)), (Bloco, (2, 2)), (Bloco, (3, 2)), (Bloco, (4, 2)), (Bloco, (5, 2)) ]

mapa07 :: Mapa
mapa07 =
  [ [Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Porta, Vazio, Caixa, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista08 :: [(Peca, Coordenadas)]
lista08 = [ (Bloco, (2, 0)), (Porta, (0, 1)), (Caixa, (2, 1)), (Bloco, (0, 2)), (Bloco, (1, 2)), (Bloco, (2, 2)), (Bloco, (3, 2)), (Bloco, (4, 2)), (Bloco, (5, 2)) ]

mapa08 :: Mapa
mapa08 =
  [ [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Porta, Vazio, Caixa, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista09 :: [(Peca, Coordenadas)]
lista09 = [ (Bloco, (0, 0)), (Bloco, (19, 0)), (Bloco, (0, 1)), (Bloco, (19, 1)), (Bloco, (0, 2)), (Bloco, (19, 2)), (Bloco, (0, 3)), (Bloco, (19, 3)), (Bloco, (0, 4)), (Bloco, (19, 4)), (Bloco, (0, 5)), (Bloco, (4, 5)), (Bloco, (12, 5)), (Bloco, (19, 5)), (Bloco, (0, 6)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Caixa, (15, 6)), (Bloco, (19, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (14, 7)), (Bloco, (15, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

mapa09 :: Mapa
mapa09 =
  [ [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista10 :: [(Peca, Coordenadas)]
lista10 = [ (Bloco, (4, 5)), (Bloco, (12, 5)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Caixa, (15, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (14, 7)), (Bloco, (15, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

mapa10 :: Mapa
mapa10 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista11 :: [(Peca, Coordenadas)]
lista11 = [ (Bloco, (19, 0)), (Bloco, (18, 1)), (Bloco, (20, 1)), (Bloco, (17, 2)), (Bloco, (21, 2)), (Bloco, (7, 3)), (Bloco, (16, 3)), (Bloco, (22, 3)), (Bloco, (6, 4)), (Bloco, (8, 4)), (Bloco, (15, 4)), (Bloco, (23, 4)), (Bloco, (3, 5)), (Bloco, (4, 5)), (Bloco, (5, 5)), (Bloco, (9, 5)), (Bloco, (14, 5)), (Bloco, (24, 5)), (Bloco, (2, 6)), (Bloco, (10, 6)), (Bloco, (13, 6)), (Bloco, (24, 6)), (Bloco, (1, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (24, 7)), (Bloco, (1, 8)), (Bloco, (24, 8)), (Bloco, (1, 9)), (Caixa, (23, 9)), (Bloco, (24, 9)), (Bloco, (1, 10)), (Caixa, (22, 10)), (Caixa, (23, 10)), (Bloco, (24, 10)), (Bloco, (1, 11)), (Bloco, (22, 11)), (Bloco, (23, 11)), (Bloco, (24, 11)), (Bloco, (0, 12)), (Bloco, (1, 12)), (Bloco, (6, 12)), (Bloco, (17, 12)), (Bloco, (22, 12)), (Bloco, (0, 13)), (Porta, (1, 13)), (Bloco, (6, 13)), (Caixa, (8, 13)), (Bloco, (17, 13)), (Bloco, (18, 13)), (Bloco, (19, 13)), (Bloco, (20, 13)), (Bloco, (21, 13)), (Bloco, (22, 13)), (Bloco, (0, 14)), (Bloco, (1, 14)), (Bloco, (2, 14)), (Bloco, (3, 14)), (Bloco, (4, 14)), (Bloco, (6, 14)), (Caixa, (8, 14)), (Caixa, (12, 14)), (Bloco, (15, 14)), (Bloco, (16, 14)), (Bloco, (17, 14)), (Bloco, (4, 15)), (Bloco, (6, 15)), (Caixa, (8, 15)), (Bloco, (10, 15)), (Bloco, (12, 15)), (Caixa, (13, 15)), (Bloco, (15, 15)), (Bloco, (4, 16)), (Bloco, (6, 16)), (Bloco, (7, 16)), (Bloco, (8, 16)), (Bloco, (9, 16)), (Bloco, (10, 16)), (Bloco, (11, 16)), (Bloco, (12, 16)), (Bloco, (13, 16)), (Bloco, (14, 16)), (Bloco, (15, 16)), (Bloco, (4, 17)), (Bloco, (5, 17)), (Bloco, (6, 17)) ]

mapa11 :: Mapa
mapa11 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
    [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco],
    [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Bloco, Caixa, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  ]

lista12 :: [(Peca, Coordenadas)]
lista12 = [(Bloco, (6, 1)), (Porta, (0, 2)), (Bloco, (6, 2)), (Bloco, (2, 3)), (Caixa, (4, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4))]

mapa12 :: Mapa
mapa12 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista13 :: [(Peca, Coordenadas)]
lista13 = [(Bloco, (6, 1)), (Porta, (1, 2)), (Bloco, (6, 2)), (Bloco, (2, 3)), (Caixa, (4, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4))]

mapa13 :: Mapa
mapa13 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Porta, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista14 :: [(Peca, Coordenadas)]
lista14 = [(Bloco,(7,1)),(Porta,(1,2)),(Bloco,(2,2)),(Caixa,(6,2)),(Bloco,(7,2)),(Bloco,(2,3)),(Caixa,(5,3)),(Caixa,(6,3)),(Bloco,(7,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Bloco,(5,4)),(Bloco,(6,4)),(Bloco,(7,4))]

mapa14 :: Mapa
mapa14 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Porta, Bloco, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

jogo01 :: Jogo
jogo01 = Jogo mapa01 (Jogador (6, 0) Oeste False)

jogo02 :: Jogo
jogo02 = Jogo mapa01 (Jogador (5, 3) Oeste True)

jogo03 :: Jogo
jogo03 = Jogo mapa09 (Jogador (17, 6) Este False)

jogo04 :: Jogo
jogo04 = Jogo mapa10 (Jogador (12, 4) Este False)

jogo05 :: Jogo
jogo05 = Jogo mapa11 (Jogador (17, 11) Oeste False)

jogo06 :: Jogo
jogo06 = Jogo mapa06 (Jogador (2, 1) Oeste False)

jogo07 :: Jogo
jogo07 = Jogo mapa12 (Jogador (5, 3) Oeste True)

jogo08 :: Jogo
jogo08 = Jogo mapa13 (Jogador (5, 3) Oeste True)

jogo09 :: Jogo
jogo09 = Jogo mapa14 (Jogador (2,1) Oeste True)

testsT1 =
  test
    [ "Valida Mapa 01" ~: True ~=? validaPotencialMapa lista01
    , "Valida Mapa 02" ~: True ~=? validaPotencialMapa lista02
    , "Valida Mapa 03" ~: True ~=? validaPotencialMapa lista03
    , "Valida Mapa 04" ~: True ~=? validaPotencialMapa lista04
    , "Valida Mapa 05" ~: True ~=? validaPotencialMapa lista05
    , "Valida Mapa 06" ~: True ~=? validaPotencialMapa lista06
    , "Valida Mapa 07" ~: True ~=? validaPotencialMapa lista07
    , "Valida Mapa 08" ~: True ~=? validaPotencialMapa lista08
    , "Valida Mapa 09" ~: True ~=? validaPotencialMapa lista09
    , "Valida Mapa 10" ~: True ~=? validaPotencialMapa lista10
    , "Valida Mapa 11" ~: True ~=? validaPotencialMapa lista11
    , "Valida Mapa 12" ~: True ~=? validaPotencialMapa lista12
    , "Valida Mapa 13" ~: True ~=? validaPotencialMapa lista13
    , "Valida Mapa 14" ~: True ~=? validaPotencialMapa lista14
    , "Invalido 01: não tem caminho" ~: False ~=? validaPotencialMapa invalido01
    , "Invalido 02: não tem caminho" ~: False ~=? validaPotencialMapa invalido02
    , "Invalido 03: caixa a flutuar" ~: False ~=? validaPotencialMapa invalido03
    , "Invalido 04: não tem caminho" ~: False ~=? validaPotencialMapa invalido04 -- um pouco lento
    , "Invalido 05: tem duas Portas" ~: False ~=? validaPotencialMapa invalido05
    , "Invalido 06: não tem porta" ~: False ~=? validaPotencialMapa invalido06
    , "Invalido 07: não tem espaços vazios" ~: False ~=? validaPotencialMapa invalido07
    , "Invalido 08: tem peças sobrepostas" ~: False ~=? validaPotencialMapa invalido08
    ]

testsT2 =
  test
    [ "Construir Mapa 01" ~: mapa01 ~=? constroiMapa lista01
    , "Construir Mapa 02" ~: mapa02 ~=? constroiMapa lista02
    , "Construir Mapa 03" ~: mapa03 ~=? constroiMapa lista03
    , "Construir Mapa 04" ~: mapa04 ~=? constroiMapa lista04
    , "Construir Mapa 05" ~: mapa05 ~=? constroiMapa lista05
    , "Construir Mapa 06" ~: mapa06 ~=? constroiMapa lista06
    , "Construir Mapa 07" ~: mapa07 ~=? constroiMapa lista07
    , "Construir Mapa 08" ~: mapa08 ~=? constroiMapa lista08
    , "Construir Mapa 09" ~: mapa09 ~=? constroiMapa lista09
    , "Construir Mapa 10" ~: mapa10 ~=? constroiMapa lista10
    , "Construir Mapa 11" ~: mapa11 ~=? constroiMapa lista11
    , "Construir Mapa 12" ~: mapa12 ~=? constroiMapa lista12
    , "Construir Mapa 13" ~: mapa13 ~=? constroiMapa lista13
    , "Construir Mapa 14" ~: mapa14 ~=? constroiMapa lista14
    , "Desconstroi Mapa 01" ~: sort lista01 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa01
    , "Desconstroi Mapa 02" ~: sort lista02 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa02
    , "Desconstroi Mapa 03" ~: sort lista03 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa03
    , "Desconstroi Mapa 04" ~: sort lista04 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa04
    , "Desconstroi Mapa 05" ~: sort lista05 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa05
    , "Desconstroi Mapa 06" ~: sort lista06 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa06
    , "Desconstroi Mapa 07" ~: sort lista07 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa07
    , "Desconstroi Mapa 08" ~: sort lista08 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa08
    , "Desconstroi Mapa 09" ~: sort lista09 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa09
    , "Desconstroi Mapa 10" ~: sort lista10 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa10
    , "Desconstroi Mapa 11" ~: sort lista11 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa11
    , "Desconstroi Mapa 12" ~: sort lista12 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa12
    , "Desconstroi Mapa 13" ~: sort lista13 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa13
    , "Desconstroi Mapa 14" ~: sort lista14 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa14
    , "Identidade Mapa 01" ~: mapa01 ~=? (constroiMapa . desconstroiMapa) mapa01
    , "Identidade Mapa 02" ~: mapa02 ~=? (constroiMapa . desconstroiMapa) mapa02
    , "Identidade Mapa 03" ~: mapa03 ~=? (constroiMapa . desconstroiMapa) mapa03
    , "Identidade Mapa 04" ~: mapa04 ~=? (constroiMapa . desconstroiMapa) mapa04
    , "Identidade Mapa 05" ~: mapa05 ~=? (constroiMapa . desconstroiMapa) mapa05
    , "Identidade Mapa 06" ~: mapa06 ~=? (constroiMapa . desconstroiMapa) mapa06
    , "Identidade Mapa 07" ~: mapa07 ~=? (constroiMapa . desconstroiMapa) mapa07
    , "Identidade Mapa 08" ~: mapa08 ~=? (constroiMapa . desconstroiMapa) mapa08
    , "Identidade Mapa 09" ~: mapa09 ~=? (constroiMapa . desconstroiMapa) mapa09
    , "Identidade Mapa 10" ~: mapa10 ~=? (constroiMapa . desconstroiMapa) mapa10
    , "Identidade Mapa 11" ~: mapa11 ~=? (constroiMapa . desconstroiMapa) mapa11
    , "Identidade Mapa 12" ~: mapa12 ~=? (constroiMapa . desconstroiMapa) mapa12
    , "Identidade Mapa 13" ~: mapa13 ~=? (constroiMapa . desconstroiMapa) mapa13
    , "Identidade Mapa 14" ~: mapa14 ~=? (constroiMapa . desconstroiMapa) mapa14
    ]

testsT3 =
  test
    [ "Show Jogo 01" ~: "      <\n      X\n      X\nP X C X\nXXXXXXX" ~=?  show jogo01
    , "Show Jogo 02" ~: "       \n      X\n     CX\nP X C<X\nXXXXXXX" ~=?  show jogo02
    , "Show Jogo 03" ~: "X                  X\nX                  X\nX                  X\nX                  X\nX                  X\nX   X       X      X\nXP  X   X C X  C > X\nXXXXXXXXXXXXXXXXXXXX" ~=? show jogo03
    , "Show Jogo 04" ~: "                    \n                    \n                    \n                    \n            >       \n    X       X       \n P  X   X C X  C    \nXXXXXXXXXXXXXXXXXXXX" ~=? show jogo04
    , "Show Jogo 04" ~: "                    \n                    \n                    \n                    \n            >       \n    X       X       \n P  X   X C X  C    \nXXXXXXXXXXXXXXXXXXXX" ~=? show jogo04
    , "Show Jogo 05" ~: "                   X     \n                  X X    \n                 X   X   \n       X        X     X  \n      X X      X       X \n   XXX   X    X         X\n  X       X  X          X\n X         XX           X\n X                      X\n X                     CX\n X                    CCX\n X               <    XXX\nXX    X          X    X  \nXP    X C        XXXXXX  \nXXXXX X C   C  XXX       \n    X X C X XC X         \n    X XXXXXXXXXX         \n    XXX                  " ~=? show jogo05
    , "Show Jogo 06" ~: "       \n  <   X\n  X   X\nP XC  X\nXXXXXXX" ~=? show jogo06
    , "Show Jogo 07" ~: "       \n      X\nP    CX\n  X C<X\nXXXXXXX" ~=? show jogo07
    , "Show Jogo 08" ~: "       \n      X\n P   CX\n  X C<X\nXXXXXXX" ~=? show jogo08
    ]

testsT4 =
  test
    [ "Movimento Trepar no Jogo 01" ~: jogo01 ~=? moveJogador jogo01 Trepar
    , "Movimento Trepar no Jogo 02" ~: Jogo mapa01 (Jogador (4, 2) Oeste True) ~=? moveJogador jogo02 Trepar
    , "Movimento Trepar Porta" ~: Jogo mapa06 (Jogador (1, 3) Oeste False) ~=? moveJogador (Jogo mapa06 (Jogador (1, 3) Oeste False)) Trepar
    , "Movimento Trepar para cima bloco" ~: Jogo mapa05 (Jogador (2, 2) Este False) ~=? moveJogador (Jogo mapa05 (Jogador (1, 3) Este False)) Trepar
    , "Movimento Trepar para Este contra parede" ~: Jogo mapa01 (Jogador (5, 3) Este False) ~=? moveJogador (Jogo mapa01 (Jogador (5, 3) Este False)) Trepar
    , "Movimento Trepar para Este com caixa" ~: Jogo mapa01 (Jogador (4, 2) Este True) ~=? moveJogador (Jogo mapa01 (Jogador (4, 2) Este True)) Trepar
    , "Movimento Trepar para Oeste para cima de Caixa" ~: Jogo mapa01 (Jogador (4, 2) Oeste False) ~=? moveJogador (Jogo mapa01 (Jogador (5, 3) Oeste False)) Trepar
    , "Movimento Trepar para Este para cima de Caixa" ~: Jogo mapa01 (Jogador (4, 2) Este False) ~=? moveJogador (Jogo mapa01 (Jogador (3, 3) Este False)) Trepar
    , "Movimento Trepar para Oeste para cima de Caixa com Caixa" ~: Jogo mapa01 (Jogador (4, 2) Oeste True) ~=? moveJogador (Jogo mapa01 (Jogador (5, 3) Oeste True)) Trepar
    , "Movimento Trepar para Este para cima de Caixa com Caixa" ~: Jogo mapa01 (Jogador (4, 2) Este True) ~=? moveJogador (Jogo mapa01 (Jogador (3, 3) Este True)) Trepar
    , "Movimento Trepar para Oeste com Caixa bloqueda por bloco" ~: Jogo mapa05 (Jogador (3, 3) Oeste True) ~=? moveJogador (Jogo mapa05 (Jogador (3, 3) Oeste True)) Trepar
    , "Movimento Trepar para Oeste para entre blocos" ~: Jogo mapa05 (Jogador (2, 2) Oeste False) ~=? moveJogador (Jogo mapa05 (Jogador (3, 3) Oeste False)) Trepar
    , "Movimento AndarEsquerda bloqueado pela caixa contra bloco" ~: Jogo mapa07 (Jogador (4, 1) Oeste True) ~=? moveJogador (Jogo mapa07 (Jogador (4, 1) Oeste True)) AndarEsquerda
    , "Movimento AndarEsquerda no Jogo 03" ~: Jogo mapa09 (Jogador (16, 6) Oeste False) ~=? moveJogador jogo03 AndarEsquerda
    , "Movimento AndarEsquerda para cima caixa" ~: Jogo mapa09 (Jogador (11, 6) Oeste False) ~=? moveJogador (Jogo mapa09 (Jogador (12, 4) Este False)) AndarEsquerda
    , "Movimento AndarEsquerda para cima caixa com caixa" ~: Jogo mapa09 (Jogador (11, 6) Oeste True) ~=? moveJogador (Jogo mapa09 (Jogador (12, 4) Este True)) AndarEsquerda
    , "Movimento AndarEsquerda de um muro" ~: Jogo mapa01 (Jogador (5,3) Oeste False) ~=? moveJogador jogo01 AndarEsquerda
    , "Movimento AndarDireita no Jogo 03" ~: Jogo mapa09 (Jogador (18, 6) Este False) ~=? moveJogador jogo03 AndarDireita
    , "Movimento AndarDireita no Jogo 04" ~: Jogo mapa10 (Jogador (13, 6) Este False) ~=? moveJogador jogo04 AndarDireita
    , "Movimento AndarDireita no Jogo 06" ~: Jogo mapa06 (Jogador (3, 2) Este False) ~=? moveJogador jogo06 AndarDireita
    , "Movimento AndarDireita descer de Caixa com Caixa" ~: Jogo mapa06 (Jogador (4, 3) Este True) ~=? moveJogador (Jogo mapa06 (Jogador (3, 2) Este True)) AndarDireita
    , "Movimento AndarDireita contra bloco" ~: Jogo mapa05 (Jogador (1, 3) Este False) ~=? moveJogador (Jogo mapa05 (Jogador (1, 3) Este False)) AndarDireita
    , "Movimento AndarDireita de um muro" ~: Jogo mapa10 (Jogador (13,6) Este False) ~=? moveJogador jogo04 AndarDireita
    , "Movimento larga Caixa" ~: Jogo (insertAt (3,3) Caixa mapa04) (Jogador (4, 3) Oeste False) ~=? moveJogador (Jogo mapa04 (Jogador (4, 3) Oeste True)) InterageCaixa
    , "Movimento tenta largar caixa Oeste" ~: Jogo mapa04 (Jogador (3, 3) Oeste True) ~=? moveJogador (Jogo mapa04 (Jogador (3, 3) Oeste True)) InterageCaixa
    , "Movimento tenta pegar Caixa Este" ~: Jogo mapa04 (Jogador (4, 3) Oeste False) ~=? moveJogador (Jogo mapa04 (Jogador (4, 3) Oeste False)) InterageCaixa
    , "Movimento levanta Caixa" ~: Jogo mapa04 (Jogador (4, 3) Oeste False) ~=? moveJogador (Jogo mapa04 (Jogador (4, 3) Oeste False)) InterageCaixa
    , "Movimento levanta Caixa" ~: Jogo (insertAt (2,1) Vazio mapa07) (Jogador (1, 1) Este True) ~=?  moveJogador (Jogo mapa07 (Jogador (1, 1) Este False)) InterageCaixa
    , "Movimento tenta levantar Caixa" ~: Jogo mapa07 (Jogador (4, 1) Este False) ~=?  moveJogador (Jogo mapa07 (Jogador (4, 1) Este False)) InterageCaixa
    , "Movimento larga Caixa entre blocos" ~: Jogo (insertAt (2, 2) Caixa mapa05) (Jogador (3, 3) Oeste False) ~=? moveJogador (Jogo mapa05 (Jogador (3, 3) Oeste True)) InterageCaixa
    , "Movimento larga Caixa" ~: Jogo (insertAt (4, 3) Caixa mapa05) (Jogador (3, 3) Este False) ~=? moveJogador (Jogo mapa05 (Jogador (3, 3) Este True)) InterageCaixa
    , "Movimento tenta largar Caixa" ~: Jogo mapa05 (Jogador (5, 3) Este True) ~=? moveJogador (Jogo mapa05 (Jogador (5, 3) Este True)) InterageCaixa
    , "Movimento tenta largar Caixa em cima de Porta Oeste" ~: Jogo mapa01 (Jogador (1,3) Oeste True) ~=? moveJogador (Jogo mapa01 (Jogador (1,3) Oeste True)) InterageCaixa
    , "Movimento tenta largar Caixa em cima de Porta Este" ~: Jogo mapa10 (Jogador (0,6) Este True) ~=?  moveJogador (Jogo mapa10 (Jogador (0,6) Este True)) InterageCaixa
    , "Movimento largar Caixa para baixo de Porta" ~: Jogo (insertAt (1,3) Caixa mapa14) (Jogador (2,1) Oeste False) ~=?  moveJogador (Jogo mapa14 (Jogador (2,1) Oeste True)) InterageCaixa
    , "Movimento tenta levantar Caixa bloqueada por bloco" ~: Jogo mapa08 (Jogador (3, 1) Oeste False) ~=? moveJogador (Jogo mapa08 (Jogador (3, 1) Oeste False)) InterageCaixa
    , "Movimento levanta Caixa" ~: Jogo (insertAt (4,3) Vazio mapa12) (Jogador (5, 3) Oeste True) ~=? moveJogador (Jogo mapa12 (Jogador (5, 3) Oeste False)) InterageCaixa
    , "Resolve Jogo 01" ~: Jogo mapa01 (Jogador (0, 3) Oeste False) ~=? correrMovimentos jogo01 [AndarEsquerda, Trepar, AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "Resolve Jogo 07" ~: Jogo (insertAt (0,3) Caixa mapa12) (Jogador (0, 2) Oeste False) ~=? correrMovimentos jogo07 [Trepar, AndarEsquerda, Trepar, AndarEsquerda, InterageCaixa, Trepar]
    , "Resolve Jogo 03" ~: Jogo (foldr (uncurry insertAt) mapa09 [((10, 6), Vazio), ((15 ,6), Vazio), ((5, 6), Caixa), ((13, 6), Caixa)]) (Jogador (1, 6) Oeste False) ~=?  correrMovimentos jogo03 [AndarEsquerda, InterageCaixa, AndarEsquerda, AndarEsquerda, InterageCaixa, Trepar, Trepar, AndarEsquerda, InterageCaixa, AndarEsquerda, AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda, InterageCaixa, Trepar, Trepar, AndarEsquerda, AndarEsquerda, AndarEsquerda]
    , "Passear no Jogo 09" ~: Jogo mapa14 (Jogador (0,3) Oeste True) ~=? correrMovimentos jogo09 [AndarEsquerda, InterageCaixa, InterageCaixa, AndarEsquerda ]
    ]

main = do
  fileT1 <- openFile "errors-T1.txt" WriteMode
  (countT1, out2) <- runTestText (putTextToHandle fileT1 False) $ TestList [testsT1]
  -- countT1 <- runTestTT $ TestList [testsT1]
  writeFile "results-T1.txt" (show countT1)

  fileT2 <- openFile "errors-T2.txt" WriteMode
  (countT2, out2) <- runTestText (putTextToHandle fileT2 False) $ TestList [testsT2]
  -- countT2 <- runTestTT $ TestList [testsT2]
  writeFile "results-T2.txt" (show countT2)

  fileT3 <- openFile "errors-T3.txt" WriteMode
  (countT3, out3) <- runTestText (putTextToHandle fileT3 False) $ TestList [testsT3]
  -- countT3 <- runTestTT $ TestList [testsT3]
  writeFile "results-T3.txt" (show countT3)

  fileT4 <- openFile "errors-T4.txt" WriteMode
  (countT4, out4) <- runTestText (putTextToHandle fileT4 False) $ TestList [testsT4]
  -- countT4 <- runTestTT $ TestList [testsT4]
  writeFile "results-T4.txt" (show countT4)

  file <- openFile "errors.txt" WriteMode
  (count, out) <- runTestText (putTextToHandle file False) $ TestList [testsT1, testsT2, testsT3, testsT4]
  -- count <- runTestTT $ TestList [testsT1, testsT2, testsT3, testsT4]
  writeFile "results.txt" (show count)

  when (failures count > 0) Exit.exitFailure

