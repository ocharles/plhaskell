plhaskell.so: PlHaskell.hs plhaskell.c
	ghc -optc -g -I/nix/store/5yx33l0aix8f0dh5gldg21ph1ghm4klb-postgresql-9.3.2/include/server -O2 plhaskell.c --make -dynamic -shared -fPIC PlHaskell.hs -o plhaskell.so -lHSrts_debug-ghc7.6.3 -lffi
