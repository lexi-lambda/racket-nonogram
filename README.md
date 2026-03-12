# nonogram

A pleasant user interface for solving [nonogram](https://en.wikipedia.org/wiki/Nonogram) puzzles (also known as Paint-by-Numbers, Griddlers, and Picross). Supports standard puzzles and [Picross S](https://en.wikipedia.org/wiki/Picross_S)-style “mega nonogram” puzzles. Supports networked co-op multiplayer.

Requires an installation of [Racket](https://download.racket-lang.org/) to run. With Racket installed, run the following commands from the repository root to install and build:

```sh
git submodule update --init
raco pkg install --link toolbox/toolbox-lib toolbox/toolbox-draw-lib nonogram
```

Once installed, the following command can be used to start the game:

```sh
racket -yl- nonogram/main --puzzle 'S5 001'
```

Alternatively, run `racket -yl- nonogram/main --help` to see the full list of options (including the supported formats for loading custom puzzles).

## Controls

* **Left Click** fills the clicked tile.
* **Right Click** (or **Ctrl+Left Click**) crosses out the clicked tile.
* **Middle Click** (or **Alt+Left Click**) marks the clicked tile.
* Clicking and dragging is supported to fill/cross/mark several tiles at once.
* **F12** completely clears the current puzzle.

Internally, racket-nonogram includes a line solver to automatically cross off completed clues. The following keybindings can be used to instruct the solver to try to actually solve the puzzle (which is obviously cheating, though it can be useful when designing/testing puzzles):

* **F1** runs the line solver on all rows.
* **F2** runs the line solver on all columns.
* **F3** runs the line solver repeatedly on both rows and columns until it reaches a fixed point.

Not all puzzles can be fully solved by the line solver. Some puzzles require utilizing information from both row and colum clues simultaneously, and the solver currently does not attempt to guess. (Also, the mega line solver is not currently exhaustive, though it does an okay job on most puzzles.)
