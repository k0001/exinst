{ mkDerivation, base, constraints, singletons_2_1, stdenv }:
mkDerivation {
  pname = "exinst";
  version = "0.1.2";
  src = ./.;
  libraryHaskellDepends = [ base constraints singletons_2_1 ];
  homepage = "https://github.com/k0001/exinst";
  description = "Derive instances for your existential types";
  license = stdenv.lib.licenses.bsd3;
}
