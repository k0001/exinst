{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? (import nixpkgsBootstrap {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "c6356ce381cd04c676eb0f6028e1f8395bdba323"; # release-17.09
    sha256 = "16qh1zplkzswwp49j9cly7ashz1xby2qgzkchi7prn3dwjhsych1"; }
}:

let

pkgs = import nixpkgs {};

hsPackageSetConfig = self: super: {
  exinst = self.callPackage (import ./pkg.nix) {};
};

ghc802 = pkgs.haskell.packages.ghc802.override {
  packageSetConfig = hsPackageSetConfig;
};

in { inherit (ghc802) exinst; }
