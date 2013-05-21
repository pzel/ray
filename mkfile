build:
  cabal-dev install
  mkdir -p ./bin && cp cabal-dev/bin/* ./bin

run: build
  ./bin/ray
