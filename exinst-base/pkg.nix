{ mkDerivation, base, binary, bytestring, constraints, deepseq
, exinst, hashable, lib, QuickCheck, singletons, singletons-base
, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-base";
  version = "0.9";
  src = ./.;
  libraryHaskellDepends = [
    base constraints exinst singletons singletons-base
  ];
  testHaskellDepends = [
    base binary bytestring deepseq exinst hashable QuickCheck tasty
    tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "@exinst@ support for @base@ package";
  license = lib.licenses.bsd3;
}
