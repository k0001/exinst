{ mkDerivation, base, binary, constraints, deepseq, hashable, lib
, profunctors, QuickCheck, singletons
}:
mkDerivation {
  pname = "exinst";
  version = "0.9";
  src = ./.;
  libraryHaskellDepends = [
    base binary constraints deepseq hashable profunctors QuickCheck
    singletons
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Dependent pairs and their instances";
  license = lib.licenses.bsd3;
}
