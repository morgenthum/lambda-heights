# Lambda-Heights

## Description

Lambda-Heights is a fast paced game. You play a small lambda letter which can jump and move left and right. You need to jump onto layers to move upwards. Meanwhile the screen also moves upward - so you need to hold the velocity. If the bottom of the screen touches the player, the game is over.

![Ingame screenshot](https://github.com/morgenthum/lambda-heights/blob/master/screenshot.png "Ingame")

## Features

- Games are always recorded to the local disk.
- Watch your games again by choosing the replay in the replay menu.

## Controls

- A or left arrow = Move left
- D or right arrow = Move right
- Space = Jump
- Esc = Pause / Exit

### Jump tricks

- Double your jump impulse by having a velocity higher than 750.
- If you are jumping and the bottom of the player is inside a layer, you can jump again, which results in very high jumps.

## Implementation

See the [IMPLEMENTATION.md](IMPLEMENTATION.md) file for implementation details.

## Build

You need the following software installed on your machine to build lambda-heights yourself.
- [Haskell stack](https://docs.haskellstack.org/en/stable/README/)
- SDL2 (developer library)
- SDL2-ttf
- SDL2-gfx

### Install native SDL2 libraries

#### Mac 

I recommend to use [homebrew](https://brew.sh/) to install the SDL2 libraries:
```
brew install pkg-config
brew install sdl2
brew install sdl2_ttf
brew install sdl2_gfx
```

#### Linux

I think you will have no problems to find the SDL2 packages in the package manager of your linux distribution.

#### Windows

The haskell stack comes with a msys2 environment. You can use the msys2 console to install the SDL2 libraries.

```
pacman -S pkg-config
pacman -S mingw-w64-x86_64-SDL2
pacman -S mingw-w64-x86_64-SDL2_gfx
pacman -S mingw-w64-x86_64-SDL2_ttf
```

### Build the project

Clone this repository, navigate to the root folder and run `stack build`. This should take some time to setup the project by installing the isolated compiler and all the dependencies of the project. When the process is done, you should find a executable in `.stack-work/install/xyz/bin` relative to the project folder. Copy the executable to the project root, because it needs the fonts folder.

Have fun!
