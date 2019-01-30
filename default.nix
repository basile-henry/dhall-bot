{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs.pkgs.haskell;

let dhall-src = nixpkgs.fetchgit {
        name = "dhall-haskell";
        url = https://github.com/dhall-lang/dhall-haskell/;
        rev = "3fbd0d72f85340ddcaa1c5b1ec41da768466df35";
        sha256 = "1vk3cp1dkg6zaw202nxiar3hjx237lhm68r6cpvfpl7jmvgm9b6j";
        fetchSubmodules = true;
    } + /dhall;

    dhall = lib.dontCheck (packages.ghc863.callCabal2nix "dhall" dhall-src {});
in

lib.failOnAllWarnings (packages.ghc863.callCabal2nix "dhall-bot" ./.  {
    inherit dhall;
})
