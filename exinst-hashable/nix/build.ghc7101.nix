# this file can be used with nix-build

with (import <nixpkgs> {}).pkgs;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let hs = haskell-ng.packages.ghc7101.override {
    overrides = self: super: {
      exinst = self.callPackage ../../exinst/nix/default.nix {};
      exinst-hashable = self.callPackage ./default.nix {};
    };
  };
in hs.exinst-hashable
