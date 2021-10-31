{- |
Module      : LI12122
Description : Módulo auxiliar para LI1 21/22

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2021/22.
 -}
module LI12122 (
    -- * Tipos de dados
    -- ** Básicos
  Coordenadas , Direcao(..),
    -- ** Mapas
  Mapa , Peca(..),
    -- ** Jogo
  Jogo(..) , Jogador(..) , Movimento(..)
  ) where

-- | Par de coordenadas de uma posição no 'Mapa'.
type Coordenadas = (Int, Int)

-- | Uma peça no 'Mapa'.
data Peca
  = Bloco -- ^ um bloco que é indestrutível e não movivel
  | Caixa -- ^ a caixa é como um bloco mas pode ser movida pelo 'Jogador'
  | Porta -- ^ a porta é a posição final do jogo
  | Vazio -- ^ um espaço vazio no 'Mapa'
  deriving (Show, Read, Eq, Ord)

type Mapa = [[Peca]]

-- | Direção de um 'Jogador' no 'Mapa'.
data Direcao
  = Este
  | Oeste
  deriving (Show, Read, Eq, Ord)

-- | O personagem que é controlado pelo 'Jogador'.
data Jogador =
  Jogador
    Coordenadas -- ^ a posição atual no 'Mapa'
    Direcao -- ^ a direção atual
    Bool -- ^ um booleano que indica se o 'Jogador' está a carregar uma 'Caixa' ou não
  deriving (Show, Read, Eq, Ord)

-- | O nível de um jogo, que inclui o puzzle (mapa) e o personagem (jogador).
data Jogo =
  Jogo
    Mapa -- ^ o puzzle em si
    Jogador -- ^ o personagem do jogo
  deriving (Read, Eq)

-- | Os movimentos que podem ser tomados pelo jogador em cada estado do 'Jogo'.
data Movimento
  = AndarEsquerda -- ^ a acção de andar para a esquerda
  | AndarDireita -- ^ a ação de andar para a direita
  | Trepar -- ^ a ação de trepar uma caixa ou bloco
  | InterageCaixa -- ^ a ação de pegar ou largar uma caixa
  deriving (Show, Read, Eq, Ord)
