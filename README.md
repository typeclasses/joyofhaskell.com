# joyofhaskell.com

## How to build

You will need Stack (and some other things, but they are taken care of for you
  if you have Nix integration enabled; see `shell.nix`).

First update the git submodules:

```bash
git submodule init
git submodule update
```

Then run `make` to build the site.

See the `Makefile` for other things you can do.
