{ mkDerivation, base, binary, bytestring, constraints, deepseq
, hashable, profunctors, QuickCheck, singletons, stdenv, tasty
, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst";
  version = "0.7";
  src = ./.;
  libraryHaskellDepends = [
    base binary constraints deepseq hashable profunctors QuickCheck
    singletons
  ];
  testHaskellDepends = [
    base binary bytestring constraints deepseq hashable profunctors
    QuickCheck singletons tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Dependent pairs and their instances";
  license = stdenv.lib.licenses.bsd3;
}
