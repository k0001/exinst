{ nixpkgs ? <nixpkgs> }:

let
pkgs = import nixpkgs {};
hsLib = import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; };

hsPkgs = pkgs.haskell.packages.ghc801.override {
  packageSetConfig = self: super: {
    exinst = self.callPackage (import ./exinst/default.nix) {};
    exinst-aeson = self.callPackage (import ./exinst-aeson/default.nix) {};
    exinst-bytes = self.callPackage (import ./exinst-bytes/default.nix) {};
    exinst-deepseq = self.callPackage (import ./exinst-deepseq/default.nix) {};
    exinst-hashable = self.callPackage (import ./exinst-hashable/default.nix) {};
  };
};

constituents = pkgs.releaseTools.aggregate {
  name = "exinst-constituents";
  meta.description = "Release-critical builds";
  constituents = [
    hsPkgs.exinst
    hsPkgs.exinst-aeson
    hsPkgs.exinst-bytes
    hsPkgs.exinst-deepseq
    hsPkgs.exinst-hashable
  ];
};

in {
  inherit constituents;
  inherit (hsPkgs)
    exinst
    exinst-aeson
    exinst-bytes
    exinst-deepseq
    exinst-hashable;
}


