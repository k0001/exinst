{ mkDerivation, aeson, base, bytestring, constraints, exinst
, exinst-base, lib, QuickCheck, singletons, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-aeson";
  version = "0.9";
  src = ./.;
  libraryHaskellDepends = [
    aeson base constraints exinst singletons
  ];
  testHaskellDepends = [
    aeson base bytestring exinst exinst-base QuickCheck tasty
    tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "@exinst@ support for @aeson@ package";
  license = lib.licenses.bsd3;
}
