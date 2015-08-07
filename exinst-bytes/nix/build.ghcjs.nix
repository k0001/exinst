# this file can be used with nix-build

with (import <nixpkgs> {}).pkgs;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let hs = haskell-ng.packages.ghcjs.override {
    overrides = self: super: {
      exinst = self.callPackage ../../exinst/nix/default.nix {};
      exinst-bytes = self.callPackage ./default.nix {};
    };
  };
in hs.exinst-bytes
