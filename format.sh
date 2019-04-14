find src -name '*.hs' | xargs brittany --write-mode=inplace
find src -name '*.hs' | xargs stylish-haskell --inplace