# advent2020

Running a particular puzzle:

```
cabal run advent2020 -- --day $DAY --part $PART --input_file $PATH_TO_PUZZLE_INPUT_FILE
```

For example:

```
cabal run advent2020 -- --day 4 --part 1 --input_file ./inputs/4
```

## Development

Running the auto-formatter:

```
cabal-fmt --inplace advent2020.cabal
ormolu -i $(find . -path ./dist-newstyle -prune -false -o -name '*.hs')
```

Running tests:

```
cabal install hspec-discover
cabal test
```

### GitHub Codespaces

You can run this project on GitHub Codespaces. After creating a Codespace using the project's devcontainer configuration, you'll need to do some manual steps to get the build to run:

```sh
# Install ghcup: https://www.haskell.org/ghcup/
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install development headers for GMP, which Haskell uses for arbitrary-precision arithmetic.
sudo apt-get install libgmp-dev
```

TODO: create a custom Haskell development container image so these steps are not needed.
