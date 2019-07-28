find lambda-heights-client/src -name '*.hs' | xargs brittany --write-mode=inplace
find lambda-heights-client-generic/src -name '*.hs' | xargs brittany --write-mode=inplace
find lambda-heights-core/src -name '*.hs' | xargs brittany --write-mode=inplace

find lambda-heights-client/src -name '*.hs' | xargs stylish-haskell --inplace
find lambda-heights-client-generic/src -name '*.hs' | xargs stylish-haskell --inplace
find lambda-heights-core/src -name '*.hs' | xargs stylish-haskell --inplace
