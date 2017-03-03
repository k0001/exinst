{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? (import nixpkgsBootstrap {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4db7ca85474e985b4b35093abab5e34a4cb64274"; # release-16.09
    sha256 = "1dbcclwkzphhgm6vzng4cph0haa7qbcdxdj3msdfddhv2ni2fnaw"; }
}:

let

pkgs = import nixpkgs {};

hsPackageSetConfig = self: super: {
  exinst = self.callPackage (import ./default.nix) {};
};

ghc802 = pkgs.haskell.packages.ghc802.override {
  packageSetConfig = hsPackageSetConfig;
};

in { inherit (ghc802) exinst; }
