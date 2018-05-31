{ mkDerivation, base, bytestring, constraints
, profunctors, singletons, stdenv, ghc
, tasty, tasty-hunit, tasty-quickcheck
, aeson, binary, bytes, cborg, cereal, deepseq
, hashable, serialise, QuickCheck
, flags ? {}
}:

let
lib = stdenv.lib;
flags' =
  { aeson = true;
    bytes = true;
    cereal = true;
    hashable = true;
    quickcheck = true;
    serialise = true;
  } // flags;

extraDeps =
  lib.optionals (flags'.aeson) [ aeson ] ++
  lib.optionals (flags'.bytes) [ bytes ] ++
  lib.optionals (flags'.cereal || flags'.bytes) [ cereal ] ++
  lib.optionals (flags'.hashable) [ hashable ] ++
  lib.optionals (flags'.serialise) [ serialise cborg ] ++
  lib.optionals (flags'.quickcheck) [ QuickCheck ];

in mkDerivation rec {
  pname = "exinst";
  version = "0.6";
  homepage = "https://github.com/k0001/exinst";
  description = "Recover instances for your existential types";
  license = stdenv.lib.licenses.bsd3;
  src = ./.;
  libraryHaskellDepends = extraDeps ++
    [ base binary bytestring constraints deepseq profunctors singletons ];
  testHaskellDepends = libraryHaskellDepends ++
    [ tasty tasty-hunit tasty-quickcheck ];
  configureFlags =
    lib.mapAttrsToList (k: v: "-f" + lib.optionalString (!v) "-" + k) flags';
}
