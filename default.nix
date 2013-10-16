{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) cabal hint;

in cabal.mkDerivation (self: {
  pname = "plhaskell";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ postgresql91 hint ];
})
