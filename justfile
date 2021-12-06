# Start GHCi using the ghciscript and pretty-simple
repl:
    stack ghci --package pretty-simple --ghci-options="-ghci-script ghciscript"

# Run tests for the given day and part with syntax: "DAY-PART".
test *DAYPART:
    stack test --test-arguments="--match={{DAYPART}}"
# For example, `just test 11-2` will test part 2 of day 11.
# Hspec can match on test descriptions with a pattern, so it is possible to use
# argument "11-" to run both tests for a specific day, for example.

