{ mkDerivation, base, bytestring, constraints
, profunctors, singletons, stdenv, ghc
, tasty, tasty-hunit, tasty-quickcheck
, aeson, binary, bytes, cborg, cereal, deepseq
, hashable, serialise, store, QuickCheck

, hasAeson ? true
, hasBinary ? true
, hasBytes ? true
, hasCereal ? true
, hasDeepseq ? true
, hasHashable ? true
, hasQuickcheck ? true
, hasSerialise ? true
, hasStore ? !(ghc.isGhcjs or false)
}:

let
extraDeps =
  stdenv.lib.optionals (hasAeson) [ aeson ] ++
  stdenv.lib.optionals (hasBinary || hasBytes) [ binary ] ++
  stdenv.lib.optionals (hasBytes) [ bytes ] ++
  stdenv.lib.optionals (hasCereal || hasBytes) [ cereal ] ++
  stdenv.lib.optionals (hasDeepseq) [ deepseq ] ++
  stdenv.lib.optionals (hasHashable) [ hashable ] ++
  stdenv.lib.optionals (hasSerialise) [ serialise cborg ] ++
  stdenv.lib.optionals (hasStore) [ store ] ++
  stdenv.lib.optionals (hasQuickcheck) [ QuickCheck ];

in mkDerivation rec {
  pname = "exinst";
  version = "0.4";
  homepage = "https://github.com/k0001/exinst";
  description = "Recover instances for your existential types";
  license = stdenv.lib.licenses.bsd3;
  src = ./.;
  libraryHaskellDepends = extraDeps ++
    [ base bytestring constraints profunctors singletons ];
  testHaskellDepends = libraryHaskellDepends ++
    [ tasty tasty-hunit tasty-quickcheck
      aeson binary bytes cereal deepseq hashable
      serialise cborg store QuickCheck
    ];
  configureFlags =
    stdenv.lib.optionals (!hasAeson) [ "-f-aeson" ] ++
    stdenv.lib.optionals (!hasBinary) [ "-f-binary" ] ++
    stdenv.lib.optionals (!hasBytes) [ "-f-bytes" ] ++
    stdenv.lib.optionals (!hasCereal) [ "-f-cereal" ] ++
    stdenv.lib.optionals (!hasDeepseq) [ "-f-deepseq" ] ++
    stdenv.lib.optionals (!hasHashable) [ "-f-hashable" ] ++
    stdenv.lib.optionals (!hasSerialise) [ "-f-serialise" ] ++
    stdenv.lib.optionals (!hasStore) [ "-f-store" ] ++
    stdenv.lib.optionals (!hasQuickcheck) [ "-f-quickcheck" ];
}
