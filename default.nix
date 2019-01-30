{ nixpkgs ? import ./nixpkgs.nix {} }:

with nixpkgs.pkgs.haskell;

let dhall-src = nixpkgs.fetchgit {
        name = "dhall-haskell";
        url = https://github.com/dhall-lang/dhall-haskell/;
        rev = "bc4fc87043d4a0868f95b726b160806e340222ed";
        sha256 = "1c7mrphmkc63683q3s9728klchr7qiwd37v1ki0vh1n6a60fp8wn";
        fetchSubmodules = true;
    } + /dhall;

    dhall = lib.dontCheck (packages.ghc863.callCabal2nix "dhall" dhall-src {});
in

lib.failOnAllWarnings (packages.ghc863.callCabal2nix "dhall-bot" ./.  {
    inherit dhall;
})
