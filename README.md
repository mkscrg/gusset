## gusset

This is a dead-simple WAI/Warp app wrapping `git show`.

Requests that look like

```
/ref/path/to/file
```

return the results of

```shell
git show ref:path/to/file
```

Build:

```shell
cabal sandbox init
cabal install
cp .cabal-sandbox/bin/gusset somewhere/in/your/path
```

Usage, assuming a Git repo at `some/git/repo` with a file called `README`:

```shell
cd some/git/repo
gusset 3000
open http://localhost:3000/master/README
```
