{ nixpkgs ? builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.09.tar.gz",
  compiler ? "ghc822"
}:

let


pkgs = import nixpkgs {};

th-desugar-src = pkgs.fetchFromGitHub {
  owner = "goldfirere";
  repo = "th-desugar";
  rev = "f3f23bec71db8b13544773d0190fc028c1c716d2";
  sha256 = "1bn2qx533k4vf1lf329hl8v6kf5n1kar4psb9q3ax7bgs85k5icz";
};

hsPackageSetConfig = self: super: {
  th-desugar = self.callCabal2nix "th-desugar" th-desugar-src  {};
  singletons = pkgs.haskell.lib.dontCheck (self.callHackage "singletons" "2.3.1" {});
  exinst = self.callPackage (import ./pkg.nix) {};
};

ghcV = pkgs.haskell.packages.${compiler}.override {
  packageSetConfig = hsPackageSetConfig;
};

in { inherit (ghcV) exinst; }
