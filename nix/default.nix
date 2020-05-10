let
  sources = import ./sources.nix;

  ghc-overrides = pkgs: self: super:
    let
      inherit (pkgs.lib) flip;
      hs = pkgs.haskell.lib;
    in {
      exinst = super.callPackage ../exinst/pkg.nix { };
      exinst-aeson = super.callPackage ../exinst-aeson/pkg.nix { };
      exinst-bytes = super.callPackage ../exinst-bytes/pkg.nix { };
      exinst-cereal = super.callPackage ../exinst-cereal/pkg.nix { };
      exinst-serialise = super.callPackage ../exinst-serialise/pkg.nix { };

      _shell = super.shellFor {
        withHoogle = false;
        packages = p: [
          p.exinst
          p.exinst-aeson
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
