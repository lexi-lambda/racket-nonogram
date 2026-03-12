# nonogram

A pleasant user interface for solving [nonogram](https://en.wikipedia.org/wiki/Nonogram) puzzles (also known as Paint-by-Numbers, Griddlers, and Picross). Supports standard puzzles and [Picross S](https://en.wikipedia.org/wiki/Picross_S)-style “mega nonogram” puzzles. Supports networked co-op multiplayer.

Requires an installation of [Racket](https://download.racket-lang.org/). With Racket installed, run the following commands from the repository root to install and build:

```sh
git submodule update --init
raco pkg install --link toolbox/toolbox-lib toolbox/toolbox-draw-lib nonogram
```

Once installed, the following command can be used to start the game:

```sh
racket -yl- nonogram/main --puzzle 'S5 001'
```

Alternatively, run `racket -yl- nonogram/main --help` to see the full list of options.
