{ pkgs }:

let
hsLib = pkgs.haskell.lib;

in
# This expression can be used as a Haskell package set `packageSetConfig`:
self: super: {
  # Too constrained test dependencies
  serialise = hsLib.doJailbreak super.serialise;

  exinst = super.callPackage ./pkg.nix {};
  # Mostly here just to test whether the thing builds with flags turned off.
  exinst_no-extras = self.exinst.override {
    flags = {
      aeson = false;
      bytes = false;
      cereal = false;
      hashable = false;
      quickcheck = false;
      serialise = false;
    };
  };
}
