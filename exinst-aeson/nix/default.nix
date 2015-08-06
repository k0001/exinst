{ stdenv
, mkDerivation

# haskell deps
, aeson
, constraints
, exinst
, singletons
}:

mkDerivation {
  pname = "exinst-aeson";
  version = "0.0.1";
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  license = stdenv.lib.licenses.bsd3;
  doHaddock = true;
  buildDepends = [ aeson constraints exinst singletons ];
}
