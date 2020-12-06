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
```

Running tests:

```
cabal test
```
