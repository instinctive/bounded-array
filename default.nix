{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    myHaskellPackage = self.callPackage ./. {};
  });

in haskellPackages.developPackage {
  root = ./.;
  source-overrides = {
    myHaskellPackage = ./.; # Path to your Haskell package
  };

  modifier = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
    cabal-install
    ghcid
    ghc
  ]);
}
