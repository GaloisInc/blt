# Release Instructions

  * finalize all commits
  * merge to master
  * wait for build bots OK
  * update versions in these files:
    - `README.md`
    - `libblt/blt.h`
    - `blt.cabal`
  * generate documentation
    - C/C++ source documentation: run `doxygen`
    - Haskell haddock docs

```
cabal configure --extra-include-dirs=$PWD/env/include --extra-lib-dirs=$PWD/env/lib
cabal build
cabal haddock
rm -rf doc/haskell
mv dist/doc/html/blt doc/haskell
```

  * commit version changes and new docs
  * git tag with latest version
  * git push
  * git push --tags
  * merge master to develop
