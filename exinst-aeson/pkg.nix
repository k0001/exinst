{ mkDerivation, aeson, base, bytestring, constraints, exinst
, QuickCheck, singletons, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-aeson";
  version = "0.7";
  src = ./.;
  libraryHaskellDepends = [
    aeson base constraints exinst singletons
  ];
  testHaskellDepends = [
    aeson base bytestring exinst QuickCheck tasty tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Dependent pairs and their instances";
  license = stdenv.lib.licenses.bsd3;
}
