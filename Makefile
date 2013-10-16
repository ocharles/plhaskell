plhaskell.so: library_init.c PlHaskell.hs
	ghc -O2 '-DMODULE=PlHaskell' library_init.c --make -dynamic -shared -fPIC PlHaskell.hs -o plhaskell.so -lHSrts_debug-ghc7.6.3
