{ compiler ? "default" }:

with (import <nixpkgs> {});
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let

  haskellPackages0 =
    if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler};
  drv = haskellPackages0.callPackage ./default.nix {};
in if pkgs.lib.inNixShell then drv.env else drv


