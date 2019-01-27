{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs.pkgs.haskell;

lib.failOnAllWarnings (packages.ghc863.callCabal2nix "dhall-bot" ./.  {
})
