{ mkDerivation, base, binary, bytes, bytestring, cereal
, constraints, exinst, exinst-cereal, lib, QuickCheck, singletons
, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-bytes";
  version = "0.7.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytes constraints exinst singletons
  ];
  testHaskellDepends = [
    base binary bytes bytestring cereal exinst exinst-cereal QuickCheck
    tasty tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Dependent pairs and their instances";
  license = lib.licenses.bsd3;
}
