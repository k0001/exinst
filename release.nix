{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? (import nixpkgsBootstrap {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "2231575a7ddd768fbce7d8f0c75926e5f1ee5c95"; # release-17.09
    sha256 = "0w55wvv55j88b3hzhjsi19g8n7vl4mng701qa38g1p3gjqsqqjsv"; }
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
