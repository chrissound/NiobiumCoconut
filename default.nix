{
  nixpkgs ? import <nixpkgs> {}
, sources ? import ./nix/sources.nix
, compiler ? "ghc864" } :

let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
    };
  };
in
  myHaskellPackages.callCabal2nix "niobiumcoconut" (./.) {}

