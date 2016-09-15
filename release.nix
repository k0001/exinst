{ nixpkgs ? <nixpkgs> }:

let
  pkgs = import nixpkgs {};
  hsLib = import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; };

  hsPkgs = pkgs.haskell.packages.ghc7103.override {
    packageSetConfig = self: super: {
      singletons_2_1 = hsLib.overrideCabal super.singletons (drv: {
        version = "2.1";
        editedCabalFile = null;
        sha256 = "0b213bn1zsjv57xz4460jxs0i85xd5i462v00iqzfb5n6sx99cmr";
      });
      exinst = self.callPackage (import ./exinst/default.nix) {};
      exinst-aeson = self.callPackage (import ./exinst-aeson/default.nix) {};
      exinst-bytes = self.callPackage (import ./exinst-bytes/default.nix) {};
      exinst-deepseq = self.callPackage (import ./exinst-deepseq/default.nix) {};
      exinst-hashable = self.callPackage (import ./exinst-hashable/default.nix) {};
    };
  };

in {
  inherit (hsPkgs)
    exinst
    exinst-aeson
    exinst-bytes
    exinst-deepseq
    exinst-hashable;
}


