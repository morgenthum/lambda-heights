# Implementation

## State machine

The game is a finite state machine. There is a state for the main menu, replay menu, playing and so on.

Each state needs to provide an implementation of the four phases of the game loop.

## Game loop

The game is a continuous repetition of the four following phases:

- Input handling (IO)
- State updating (Pure)
- Output handling (IO)
- Rendering (IO)

In games that are developed in imperative languages, the state updating and output handling phases are usually combined. Lambda-Heights has been written in Haskell, which is a purely functional language. It aims to separate IO from pure code. This led to the decision to separate these phases. State updating is pure code which forbids any IO code, while output handling needs, as the name implies, IO code.

The game runs frame rate independent. The state is updated with a fixed rate. The loop measures the elapsed time in the real world and executes the updating cycle as often as needed to catch up the real time.

### Phases

#### Input handling

Type: `type Input m e = m e`

Reads and converts needed events for the next phase.

#### State updating

Type: `type Update s r e = LoopTimer -> e -> s -> Either r s`

The updater is equipped with:
- the timer, which gives access to timing properties like frames per second, elapsed time etc.
- the events collected by the input handling phase
- the current state

and it returns either the state for next cycle, or the result - which implies the state has come to an end.

#### Output handling

Type: `type Output m s r e = LoopTimer -> e -> Either r s -> m ()`

It's quite the same as the state updating phase, but it's boosted by a monad `m`, which is principally the `IO` monad.

#### Rendering

Type: `type Render m s = LoopTimer -> s -> m ()`

The rendering phase is equipped with:
- the timer, as usual
- and the state it renders

## Record game

The play state broadcasts in the output handling phase all occurred events into a transactional channel. A separate thread reads from the channel and serializes to the local disk.

## Replay

The replay state applies all events of the replay file to the state updating and rendering phase of the play state.

## User interface

The user interface consists principally of
- list views (main menu, pause menu, etc.)
- table views (overview in replay menu). 

Both are implemented the same way - as table. The table is a matrix that contains the texts and a 2 dimensional vector that points to the selected item.

The selection updater of the table can be configured to limit the selectable cells. For example: The head of a table can not be selected.

When it comes to the rendering phase, different matrices are built from the table to specify the
- styling of the cells (back and foreground)
- positioning of the cells (list, grid - with gaps, and so on)
- sizing of the cells (sizing by content size, aligning widths of a whole column, etc.)

These matrices are combined to a matrix which can be rendered.

The generation of the styling, positioning and sizing matrices can be configured by using the combinator pattern.

Thats the configuration of the replay overview table:
```
newTableView :: SDLF.Font -> Table -> IO TableView
newTableView font table = do
  let contents = cellText <$> content table
  fontSizes <- loadFontSizes font contents
  let headStyle           = always $ CellStyle font (V4 0 191 255 255) (V4 30 30 30 255)
  let selectedStyle       = always $ CellStyle font (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle           = always $ CellStyle font (V4 30 30 30 255) (V4 255 255 255 255)
  let selectedOrBodyStyle = ifSelector selectedRow selectedStyle bodyStyle
  let styles              = (styleWith $ ifSelector (row 1) headStyle selectedOrBodyStyle) table
  let sizes               = (alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes) table
  let positions           = (locateWith $ indent selectedRow (V2 10 0) $ addGaps (V2 20 20) $ grid sizes) table
  let textPositions       = (locateTextWith $ centerText sizes positions fontSizes) table
  return $ merge contents styles sizes positions textPositions
```