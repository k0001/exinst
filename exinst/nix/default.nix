{ stdenv
, mkDerivation

# haskell deps
, constraints
, singletons
}:

mkDerivation {
  pname = "exinst";
  version = "0.0.1";
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  license = stdenv.lib.licenses.bsd3;
  doHaddock = true;
  buildDepends = [ constraints singletons ];
}
