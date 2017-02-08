{ mkDerivation, aeson, base, bytes, constraints, generic-random
, profunctors, QuickCheck, singletons, stdenv, tasty, tasty-hunit
, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst";
  version = "0.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytes constraints profunctors QuickCheck singletons
  ];
  testHaskellDepends = [
    aeson base bytes constraints generic-random profunctors QuickCheck
    singletons tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Derive instances for your existential types";
  license = stdenv.lib.licenses.bsd3;
}
