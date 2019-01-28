{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs.pkgs.haskell;

let dhall-haskell = import (builtins.fetchGit {
        name = "dhall-haskell";
        url = https://github.com/dhall-lang/dhall-haskell/;
        rev = "2d2c897e3f43a9b1188ae82e7a1da5b5218fb135";
        ref = "master";
    });
in

lib.failOnAllWarnings (packages.ghc863.callCabal2nix "dhall-bot" ./.  {
    dhall = dhall-haskell.dhall;
})
