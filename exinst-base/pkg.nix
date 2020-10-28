{ mkDerivation, base, bytestring, constraints, deepseq, exinst
, hashable, profunctors, QuickCheck, singletons, singletons-base
, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-base";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base constraints deepseq exinst hashable profunctors QuickCheck
    singletons singletons-base
  ];
  testHaskellDepends = [
    base bytestring constraints deepseq exinst profunctors QuickCheck
    singletons tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Dependent pairs and their instances (from the base library)";
  license = stdenv.lib.licenses.bsd3;
}
