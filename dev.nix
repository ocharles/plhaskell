with import <nixpkgs> {};
let
  customHaskellPackages = import <nixpkgs/pkgs/top-level/haskell-packages.nix> {
    inherit pkgs newScope;
    ghc = haskellPackages.ghc;
    prefFun = self: super: self // self.haskellPlatformArgs_2013_2_0_0 self // {
      haskellPlatform = self.haskellPlatform_2013_2_0_0;
      extensibleExceptions = self.extensibleExceptions_0_1_1_4;
      hint = self.callPackage ./hint.nix {};
      plhaskell = self.callPackage ./. { postgresql = postgresql93; };
    };
    enableSharedLibraries = true;
  };

in customHaskellPackages.plhaskell