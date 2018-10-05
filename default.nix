# This file exports every derivation introduced by this repository.
{ nixpkgs ? import ./nixpkgs.nix }:
let pkgs = import ./pkgs.nix { inherit nixpkgs; };
in
pkgs.releaseTools.aggregate {
  name = "everything";
  constituents = [
    pkgs._here.ghc861.exinst
    pkgs._here.ghc861.exinst.doc
    pkgs._here.ghc861.exinst-aeson
    pkgs._here.ghc861.exinst-aeson.doc
    pkgs._here.ghc861.exinst-bytes
    pkgs._here.ghc861.exinst-bytes.doc
    pkgs._here.ghc861.exinst-cereal
    pkgs._here.ghc861.exinst-cereal.doc
    pkgs._here.ghc861.exinst-serialise
    pkgs._here.ghc861.exinst-serialise.doc
    pkgs._here.ghc861._shell
  ];
}

