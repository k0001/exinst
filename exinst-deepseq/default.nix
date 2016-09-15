{ mkDerivation, base, constraints, deepseq, exinst, stdenv }:
mkDerivation {
  pname = "exinst-deepseq";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [ base constraints deepseq exinst ];
  homepage = "https://github.com/k0001/exinst";
  description = "Derive instances for the `deepseq` library for your existential types";
  license = stdenv.lib.licenses.bsd3;
}
