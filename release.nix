{ nixpkgs ? (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "d982c61f1afcac2f7f99fc9740c031c1bc02e456"; # unstable, jan 14 2018
    sha256 = "0sq6fa10sqm7pnk84kbpzv4sjz2dza3jlk43cl1b04aqn0yfspnq"; }
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
  exinst = self.callPackage (import ./pkg.nix) {};
  exinst_no-extras = self.exinst.override {
    hasAeson = false;
    hasBinary = false;
    hasBytes = false;
    hasCereal = false;
    hasDeepseq = false;
    hasHashable = false;
    hasQuickcheck = false;
    hasSerialise = false;
  };
};

ghc822 = pkgs.haskell.packages.ghc822.override {
  packageSetConfig = hsPackageSetConfig;
};

in { inherit (ghc822) exinst exinst_no-extras; }
