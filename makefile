
run: build
	./bin/ray

build:
	rm -f ./bin/ray
	cabal install
	mkdir -p ./bin && cp dist/build/ray/ray ./bin

