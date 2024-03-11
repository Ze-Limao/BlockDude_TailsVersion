# Laboratórios de Informática I
## Software Labs I | Laboratórios de Informática I
This game was developed during the first semester of the Software Engineering degree @ University of Minho.

The project consisted of recreating the old game BlockDude, with the freedom to add features and completely change the graphical appearance, which was, in this case, heavily inspired by Sonic the Hedgehog.

It was fully developed in Haskell and consisted of 6 tasks:
- Verify if a given map is valid;
- Construct and destruct maps (from type [(Peca, Coordenadas)] to type Mapa and vice-versa);
- Instantiate Show class in order to display an output of the type Jogo as a String;
- Update the current game state according to the player movements;
- Create and implement game graphics, using Gloss.
- Create a game bot to solve, in the minimum necessary player movements, a map.

### Demonstração do jogo: https://youtu.be/Qf_H0eNuY2Y

## Installing and running the game
Firstly, install Haskell Platform (GHC and Cabal). To do so, follow the instructions for your specific system at: haskell.org/downloads
Finally, since the graphical interface of the game was developed using the Gloss library, you'll need to install it:
```
$ cabal update
$ cabal install --lib gloss
$ cabal install --lib gloss-juicy
```
### Clone the repository
``` 
$ git clone git@github.com:ruilopesm/Block-Knight-LI1.git
```
### Compiling 
```
$ cd src
$ ghc Tarefa5_2021li1g033.hs
````
### Running
```
$ ./Tarefa5_2021li1g033
```
## Possible problems
In case you get an mpv error, you'll need to install it (as it was used to play audio inside the game). Follow the instructions here: mpv.io/installation
