let pkgs = import ./nix;
in pkgs.releaseTools.aggregate {
  name = "everything";
  constituents = let p = pkgs._here.ghc865;
  in [
    p.exinst
    p.exinst.doc
    p.exinst-aeson
    p.exinst-aeson.doc
    p.exinst-bytes
    p.exinst-bytes.doc
    p.exinst-cereal
    p.exinst-cereal.doc
    p.exinst-serialise
    p.exinst-serialise.doc
    p._shell
  ];
}

