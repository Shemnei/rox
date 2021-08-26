# Rox Language

Rox language is a rust implementation of the `lox` language by Bob Nystrom ([CraftingIterpreters](https://craftinginterpreters.com)).

## Examples

### File

```text
fun makeCounter() {
  var i = 0;
  fun count() {
    i = i + 1;
    print i;
  }

  return count;
}

var counter = makeCounter();
var c1 = counter(); // "1".
```

```bash
$ rox FILENAME
1
```

### REPL

To run a REPL, just call rox with no arguements.

```bash
rox
```
