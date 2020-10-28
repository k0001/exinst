let
  sources = import ./sources.nix;

  ghc-overrides = pkgs: self: super:
    let
      inherit (pkgs.lib) flip;
      hs = pkgs.haskell.lib;
    in {
      exinst = super.callPackage ../exinst/pkg.nix { };
      exinst-base = super.callPackage ../exinst-base/pkg.nix { };
      exinst-aeson = super.callPackage ../exinst-aeson/pkg.nix { };
      exinst-bytes = super.callPackage ../exinst-bytes/pkg.nix { };
      exinst-cereal = super.callPackage ../exinst-cereal/pkg.nix { };
      exinst-serialise = super.callPackage ../exinst-serialise/pkg.nix { };

#      th-lift = super.callCabal2nix "th-lift" sources.th-lift {};
#      th-abstraction = super.callCabal2nix "th-abstraction" sources.th-abstraction {};
#      th-desugar = super.callCabal2nix "th-desugar" sources.th-desugar {};
      singletons = super.callCabal2nix "singletons" "${sources.singletons}/singletons" {};
#      singletons-base = super.callCabal2nix "singletons-base" "${sources.singletons}/singletons-base" {};
#      singletons-th = super.callCabal2nix "singletons-th" "${sources.singletons}/singletons-th" {};

      _shell = super.shellFor {
        withHoogle = false;
        buildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
        packages = p: [
          p.exinst
          p.exinst-aeson
          # p.exinst-base
          p.exinst-bytes
          p.exinst-cereal
          p.exinst-serialise
        ];
      };
    };
  pkgs-overlay = self: super: {
    _here = {
      inherit sources;
      inherit (import sources.niv { }) niv;

      ghc865 = super.haskell.packages.ghc865.override {
        overrides = ghc-overrides self;
      };
    };
  };

in import sources.nixpkgs { overlays = [ pkgs-overlay ]; }
