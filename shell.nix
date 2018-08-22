{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
#{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc842" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    xmonad
    xmonad-contrib
    xmonad-extras
    yeganesh
    #    taffybar
    xmobar
  ]);
in
  pkgs.stdenv.mkDerivation {
    name = "my-haskell-env-0";
    buildInputs = [ ghc ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  }
