let
  sources = import ./sources.nix;

  ghc-overrides = pkgs: self: super:
    let
      inherit (pkgs.lib) flip;
      hs = pkgs.haskell.lib;
    in {
      exinst = super.callPackage ../exinst/pkg.nix { };
      exinst-aeson = super.callPackage ../exinst-aeson/pkg.nix { };
      exinst-base = super.callPackage ../exinst-base/pkg.nix { };
      exinst-bytes = super.callPackage ../exinst-bytes/pkg.nix { };
      exinst-cereal = super.callPackage ../exinst-cereal/pkg.nix { };
      exinst-serialise = super.callPackage ../exinst-serialise/pkg.nix { };

      chell = hs.doJailbreak super.chell;
      ListLike = hs.dontCheck super.ListLike;

      singletons = self.callHackage "singletons" "3.0.2" {};
      singletons-th = self.callHackage "singletons-th" "3.1.1" {};
      singletons-base = self.callHackage "singletons-base" "3.1.1" {};

      _shell = super.shellFor {
        withHoogle = true;
        buildInputs = [ self.cabal-install ];
        packages = p: [
          p.exinst
          p.exinst-aeson
          p.exinst-base
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

      ghc943 = super.haskell.packages.ghc943.override {
        overrides = ghc-overrides self;
      };
    };
  };

in import sources.nixpkgs { overlays = [ pkgs-overlay ]; }
