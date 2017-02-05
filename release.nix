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
  exinst = self.callPackage (import ./exinst/default.nix) {};
  exinst-aeson = self.callPackage (import ./exinst-aeson/default.nix) {};
  exinst-bytes = self.callPackage (import ./exinst-bytes/default.nix) {};
  exinst-deepseq = self.callPackage (import ./exinst-deepseq/default.nix) {};
  exinst-hashable = self.callPackage (import ./exinst-hashable/default.nix) {};
};

ghc802 = pkgs.haskell.packages.ghc801.override {
  packageSetConfig = hsPackageSetConfig;
};

constituents = pkgs.releaseTools.aggregate {
  name = "exinst-constituents";
  meta.description = "Release-critical builds";
  constituents = [
    ghc802.exinst
    ghc802.exinst-aeson
    ghc802.exinst-bytes
    ghc802.exinst-deepseq
    ghc802.exinst-hashable
  ];
};

in {
  inherit constituents;
  inherit (ghc802)
    exinst
    exinst-aeson
    exinst-bytes
    exinst-deepseq
    exinst-hashable;
}


