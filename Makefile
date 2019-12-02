default: build-all

clean:
	stack clean
	rm -rf build/ python/imf/_imf.{c,o,so}

build-all: build/libimf.so python/imf/_imf.so

test:
	stack test --pedantic --ghc-options -fno-warn-name-shadowing --ghc-options -fno-warn-orphans
	cd python/ && LD_LIBRARY_PATH=/usr/local/lib nosetests

install: build/libimf.so
	cp build/libimf.so /usr/local/lib

docs:
	stack haddock

python/imf/_imf.so:
	python/imf/_imf_build.py

build/libimf.so build/Data/IMF/FFI_stub.h:
	mkdir -p build
	stack ghc -- -dynamic -shared -fPIC -lHSrts-ghc8.4.3 -outputdir build -o build/libimf.so haskell/src/Data/IMF.hs haskell/src/Data/IMF/*.hs haskell/src/Data/IMF/Parsers/*.hs

.PHONY: default clean build-all test install docs
