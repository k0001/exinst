{ mkDerivation, base, binary, bytes, bytestring, cereal
, constraints, exinst, exinst-base, exinst-cereal, lib, QuickCheck
, singletons, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-bytes";
  version = "0.9";
  src = ./.;
  libraryHaskellDepends = [
    base bytes constraints exinst singletons
  ];
  testHaskellDepends = [
    base binary bytes bytestring cereal exinst exinst-base
    exinst-cereal QuickCheck tasty tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "@exinst@ support for @bytes@ package";
  license = lib.licenses.bsd3;
}
