{ mkDerivation, base, binary, constraints, exinst, QuickCheck
, serialise, singletons, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst-serialise";
  version = "0.7";
  src = ./.;
  libraryHaskellDepends = [
    base constraints exinst serialise singletons
  ];
  testHaskellDepends = [
    base binary exinst QuickCheck serialise tasty tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Dependent pairs and their instances";
  license = stdenv.lib.licenses.bsd3;
}
