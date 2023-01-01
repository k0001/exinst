{ mkDerivation, base, binary, bytestring, constraints, deepseq
, hashable, lib, profunctors, QuickCheck, singletons
, singletons-base, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst";
  version = "0.8";
  src = ./.;
  libraryHaskellDepends = [
    base binary constraints deepseq hashable profunctors QuickCheck
    singletons singletons-base
  ];
  testHaskellDepends = [
    base binary bytestring constraints deepseq hashable profunctors
    QuickCheck singletons tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Dependent pairs and their instances";
  license = lib.licenses.bsd3;
}
