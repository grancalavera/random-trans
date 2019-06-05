# random-trans

> This is a simple application exercise to practise monad transformers. The application
> runs with `ReaderT` at the top of the stack and performs effects in `MonadIO` and ExceptT.
> Additionally there are random computations which run in `MonadIO`

## Usage

```
random-trans --help
Usage: <interactive> [-i|--invert-parity-order]
  Validation and Random in the same program

Available options:
  -h,--help                Show this help text
  -i,--invert-parity-order Invert parity order: from odd-even (default) to
                           even-odd (inverted)
```

## Collecting more than one error

When asked to enter integer values, just enter anything except an integer, then you should
get two errors. This should prove that `Data.Validation` is working as expected.

## Collecting single errors

After the initial integer validation, all remaining errors are just single errors. This means
that after validating the input for integer values, every step of the program depends on some
previous validation, which in turn means validations must run in the `Either` monad instead of
running in the `Validation` applicative functor. Multiple errors wont be collected, since after
the first error the program cannot continue.


## Acknowledgments

Based on [Refactoring to a Monad Transformer Stack ](https://thoughtbot.com/blog/refactoring-to-a-monad-transformer-stack) by [Josh Clayton](https://thoughtbot.com/blog/authors/josh-clayton) from [thoughtbot](https://thoughtbot.com/).
