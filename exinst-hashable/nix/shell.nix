{ compiler ? "default" }:

with (import <nixpkgs> {});
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let

  haskellPackages0 =
    if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler};

  hs = haskellPackages0.override {
    overrides = self: super: {
        exinst = self.callPackage ../../exinst/nix/default.nix {};
        singletons = self.callPackage /home/k/q/singletons.git/default.nix {};
      };
    };

  drv = hs.callPackage ./default.nix {};
in if pkgs.lib.inNixShell then drv.env else drv


