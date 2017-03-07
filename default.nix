{ mkDerivation, aeson, base, binary, bytes, bytestring, cereal
, constraints, deepseq, hashable, profunctors, QuickCheck
, singletons, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytes cereal constraints deepseq hashable
    profunctors QuickCheck singletons
  ];
  testHaskellDepends = [
    aeson base binary bytes bytestring cereal constraints deepseq
    hashable profunctors QuickCheck singletons tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Derive instances for your existential types";
  license = stdenv.lib.licenses.bsd3;
}
