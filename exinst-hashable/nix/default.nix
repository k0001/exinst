{ stdenv
, mkDerivation

# haskell deps
, hashable
, constraints
, exinst
}:

mkDerivation {
  pname = "exinst-hashable";
  version = "0.0.1";
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  license = stdenv.lib.licenses.bsd3;
  doHaddock = true;
  buildDepends = [ hashable constraints exinst ];
}
