GHC_INC_DIR=/usr/lib64/ghc-7.6.3/include/

all: main

main: main.hs Transition.hs State.hs
	cabal sandbox init; cabal install --prefix=`pwd`/install; cp install/bin/scheduler .

clean:
	rm -rf dist install cabal.sandbox.config .cabal-sandbox scheduler

