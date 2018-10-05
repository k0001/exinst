{ pkgs }:

let
hsLib = pkgs.haskell.lib;
th-desugar =
  { mkDerivation, base, containers, hspec, HUnit, mtl, stdenv, syb
  , template-haskell, th-expand-syns, th-lift, th-orphans
  }:
  mkDerivation {
    pname = "th-desugar";
    version = "1.9";
    sha256 = "f14a7a854df55abb3bbca5ef0ec202ed4d7e1631a5fb51767d360dc1b604afef";
    libraryHaskellDepends = [
      base containers mtl syb template-haskell th-expand-syns th-lift
      th-orphans
    ];
    testHaskellDepends = [
      base containers hspec HUnit mtl syb template-haskell th-expand-syns
      th-lift th-orphans
    ];
    homepage = "https://github.com/goldfirere/th-desugar";
    description = "Functions to desugar Template Haskell";
    license = stdenv.lib.licenses.bsd3;
  };
singletons =
  { mkDerivation, base, Cabal, containers, directory, fetchgit
  , filepath, ghc-boot-th, mtl, pretty, process, stdenv, syb, tasty
  , tasty-golden, template-haskell, text, th-desugar, transformers
  }:
  mkDerivation {
    pname = "singletons";
    version = "2.5";
    src = fetchgit {
      url = "https://github.com/goldfirere/singletons";
      sha256 = "0dwa40bxm0j6dilnmpcrpfmbavg8c4ijrlnx96bwdsajcrdrzgfw";
      rev = "5cf4b5418ac3e54d8d8f355afc934e6d673d2b06";
    };
    setupHaskellDepends = [ base Cabal directory filepath ];
    libraryHaskellDepends = [
      base containers ghc-boot-th mtl pretty syb template-haskell text
      th-desugar transformers
    ];
    testHaskellDepends = [ base filepath process tasty tasty-golden ];
    homepage = "http://www.github.com/goldfirere/singletons";
    description = "A framework for generating singleton types";
    license = stdenv.lib.licenses.bsd3;
  };

inherit (pkgs.haskell.lib) doJailbreak;

in
# This expression can be used as a Haskell package set `packageSetConfig`:
  (self: super: {
     th-desugar = super.callPackage th-desugar {};
     th-expand-syns = doJailbreak super.th-expand-syns;
     singletons = super.callPackage singletons {};

     exinst = super.callPackage exinst/pkg.nix {};
     exinst-aeson = super.callPackage exinst-aeson/pkg.nix {};
     exinst-bytes = super.callPackage exinst-bytes/pkg.nix {};
     exinst-cereal = super.callPackage exinst-cereal/pkg.nix {};
     exinst-serialise = super.callPackage exinst-serialise/pkg.nix {};

     _shell = self.shellFor {
       packages = p: [
         p.exinst
         p.exinst-aeson
         p.exinst-bytes
         p.exinst-cereal
         p.exinst-serialise
       ];
     };
  })
