{ mkDerivation, base, binary, bytestring, cereal, constraints
, exinst, QuickCheck, singletons, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-cereal";
  version = "0.7";
  src = ./.;
  libraryHaskellDepends = [
    base cereal constraints exinst singletons
  ];
  testHaskellDepends = [
    base binary bytestring cereal exinst QuickCheck tasty
    tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Dependent pairs and their instances";
  license = stdenv.lib.licenses.bsd3;
}
