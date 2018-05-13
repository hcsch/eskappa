# Sκ
A simple re-make of the game Escapa, which can be found on multiple pages online.

The re-make is written in elm, makes use of WebGL and a very rudimentary set of html elements to display the game.

## Building

An installation of elm is required, yarn/npm are only used for comfortable development and building.

To build the HTML file after cloning the repository just execute the following chain of commands:

```bash
$ yarn install && elm-package install && yarn make
```

## Development

For development I've been using watchman, watchman-make (pip), live-server (npm) and concurrently (npm).

To use my setup just execute the following:

```bash
$ yarn dev
```

## TODO

- Make sure elm-webgl uses `DYNAMIC_DRAW` buffers for vertex data. Currently it seems that there is only an option for `STATIC_DRAW`.
- Modularize and clean up the code some more.
- Add some useful comments.
- Use elm-css (or the default elm-lang option; whichever seems the most fitting) to style HTML elements, as they are looking somewhat bland currently