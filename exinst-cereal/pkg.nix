{ mkDerivation, base, binary, bytestring, cereal, constraints
, exinst, exinst-base, lib, QuickCheck, singletons, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-cereal";
  version = "0.9";
  src = ./.;
  libraryHaskellDepends = [
    base cereal constraints exinst singletons
  ];
  testHaskellDepends = [
    base binary bytestring cereal exinst exinst-base QuickCheck tasty
    tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "@exinst@ support for @cereal@ package";
  license = lib.licenses.bsd3;
}
