{ stdenv
, mkDerivation

# haskell deps
, bytes
, constraints
, exinst
, singletons
}:

mkDerivation {
  pname = "exinst-bytes";
  version = "0.0.1";
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  license = stdenv.lib.licenses.bsd3;
  doHaddock = true;
  buildDepends = [ bytes constraints exinst singletons ];
}
