{ stdenv
, mkDerivation

# haskell deps
, deepseq
, constraints
, exinst
}:

mkDerivation {
  pname = "exinst-deepseq";
  version = "0.0.1";
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  license = stdenv.lib.licenses.bsd3;
  doHaddock = true;
  buildDepends = [ deepseq constraints exinst ];
}
