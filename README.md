# Lambda-Tower

## Description

Lambda-Tower is a fast paced arcade game. You play a small lambda letter which has the mission to escape the tower to the top. You have the ability to jump onto layers to get to the top, sounds easy - but there is the darkness which comes closer and closer to you - and it gets faster over the time.

![Ingame screenshot](https://github.com/morgenthum/lambda-tower/blob/master/screenshot.png "Ingame")

### Features

- You have the ability to the play the game. :)
- It always records your game and stores is to your hard disk drive. You can always replay your last game.

#### What is planned?

- Multiple replays (with timestamp)
- Highscore table over all players (with replays).

## Build

You need the following software installed on your machine to build lambda-tower yourself.
- [Haskell stack](https://docs.haskellstack.org/en/stable/README/)
- SDL2 (developer library)
- SDL2-ttf
- SDL2-gfx

### Install native SDL2 libraries

#### Mac 

I recommend to use [Homebrew](https://brew.sh/) to install the SDL2 libraries:
```
brew install sdl2
brew install sdl2_ttf
brew install sdl2_gfx
```

#### Linux

I think you will have no problems to find the SDL2 packages in the package manager of your linux distribution.

#### Windows

The haskell stack comes with a msys2 environment. You can use the msys2 console to install the SDL2 libraries.

```
pacman -Ss mingw-w64-SDL2
pacman -Ss mingw-w64-SDL2_gfx
pacman -Ss mingw-w64-SDL2_ttf
```

Pacman stores the pkg-config files of the libraries to `/mingw64/lib/pkgconfig` which wasn't in the lookup path on my machine by default. I had to copy the `sdl2*.pc` files to `/usr/lib/pkgconfig` by my own to enable the haskell stack to find the native libraries for the haskell binding libraries.

### Build the project

Clone this repository, navigate to the root folder and run `stack build`. This should take some time to setup the project by installing the isolated compiler and all the dependencies of the project. When the process is done, you should find a executable in `.stack-work/install/xyz/bin` relative to the project folder. You need to copy the font `HighSchoolUSASans.ttf` to the executable to get it running.

Have fun!
