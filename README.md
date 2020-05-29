# Statistic functions for lists of Float and Int

This package provides functions to get common statistic functions over lists of numbers.

If you need a function that is currently not included,
please let me know by creating an [Issue in Github](https://github.com/f0i/statistics/issues/new)
or send me an email at [elm-statistics@f0i.de](mailto:elm-statistics@f0i.de?subject=github:f0i/statistics).
Same thing for performance improvements, ideas, sponsoring and job offers.


## Install

```
elm install f0i/statistics
```


## Examples

```elm
$ elm repl

> import Statistics as Stat
-- This is the test data for the following examples:
> data = [1, 1, 2, 2, 4, 8, 8, 9]
[1,1,2,2,4,8,8,9]

> data |> Stat.avg
Just 4.375 : Maybe Float

> data |> Stat.mean -- alias for avg
Just 4.375 : Maybe Float

> data |> Stat.occurrences
Dict.fromList [(1,2),(2,2),(4,1),(8,2),(9,1)]
    : Dict.Dict number Int

> data |> Stat.minmax
Just (1,9) : Maybe ( number, number )

> data |> Stat.stdDeviation
Just 3.1991209730174317 : Maybe Float

-- Get the element 25% into the list (interpolated between closest values)
> data |> Stat.percentile 0.25
Just 1.75 : Maybe Float

-- Some functions have a separate implementation for list of Int
> data |> Stat.percentileInt 0.75
Just 8 : Maybe Int

>
```

The complete list of function definitions can be found in
[the package documentation](https://package.elm-lang.org/packages/f0i/statistics/latest/Statistics)