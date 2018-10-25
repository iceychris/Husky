# Husky

A simplistic music visualizer.

See it in [action](https://asciinema.org/a/ZBjJfRlbMM5IJDeFOzWNk1Fje).


## Dependencies

    ghc nix cabal2nix

and some libraries from [Hackage](https://hackage.haskell.org/).


## Run

Preparation:

    # run cabal2nix, fetch dependencies
    make regen

    # compile
    make build

Run via:

    make run


## Development

    make shell
    ghci

In ghci: 

``` haskell
    -- reload source file
    :l Husky

    -- run main function
    main
```


