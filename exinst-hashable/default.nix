{ mkDerivation, base, constraints, exinst, hashable, singletons
, stdenv
}:
mkDerivation {
  pname = "exinst-hashable";
  version = "0.1.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base constraints exinst hashable singletons
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Derive instances for the `hashable` library for your existential types";
  license = stdenv.lib.licenses.bsd3;
}
