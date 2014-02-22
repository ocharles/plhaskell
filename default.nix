{ cabal, postgresql, hint, libffi }:
cabal.mkDerivation (self: {
  pname = "plhaskell";
  version = "0.1.0";
  src = ./.;
  extraLibraries = [ postgresql ];
  buildDepends = [ hint ];
})
