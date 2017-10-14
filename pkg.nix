{ mkDerivation, aeson, base, base16-bytestring, base64-bytestring
, binary, bytes, bytestring, cborg, cereal, constraints, deepseq
, hashable, profunctors, QuickCheck, serialise, singletons, stdenv
, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "exinst";
  version = "0.4";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytes cborg cereal constraints deepseq hashable
    profunctors QuickCheck serialise singletons
  ];
  testHaskellDepends = [
    aeson base base16-bytestring base64-bytestring binary bytes
    bytestring cborg cereal constraints deepseq hashable profunctors
    QuickCheck serialise singletons tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/exinst";
  description = "Recover type indexes and instances for your existentialized types";
  license = stdenv.lib.licenses.bsd3;
}
