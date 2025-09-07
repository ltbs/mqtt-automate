# Dynamic Reload Example

This is a minimal Haskell application demonstrating how to use the
[`hint`](https://hackage.haskell.org/package/hint) interpreter to reload
code whenever a source file changes. The `reloader` executable watches
`Dynamic.hs` and executes the `greet` function each time the file is
modified.

## Running

```bash
stack build
stack run
```

Modify `Dynamic.hs` while the program runs to see it reload and execute
the new code.
