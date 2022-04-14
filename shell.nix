{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) haskellPackages;

  project = import ./release.nix { pkgs = pkgs; };
in
pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.haskell-language-server
    pkgs.ormolu
  ];
}
