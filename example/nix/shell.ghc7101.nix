# this file can be used with nix-shell

with (import <nixpkgs> {}).pkgs;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let hs = haskell-ng.packages.ghc7101.override {
      overrides = self: super: {
        exinst = self.callPackage ../../exinst/nix/default.nix {};
        exinst-aeson = self.callPackage ../../exinst-aeson/nix/default.nix {};
        exinst-bytes = self.callPackage ../../exinst-bytes/nix/default.nix {};
        exinst-deepseq = self.callPackage ../../exinst-deepseq/nix/default.nix {};
        exinst-hashable = self.callPackage ../../exinst-hashable/nix/default.nix {};

        singletons = haskell-ng.lib.overrideCabal super.singletons (drv: { doCheck = false; });

        instant-aeson = self.callPackage (
          { mkDerivation, base, stdenv, aeson, instant-generics
          , tasty, tasty-quickcheck }:
          mkDerivation {
            pname = "instant-aeson";
            version = "0.1";
            buildDepends = [
              base aeson instant-generics tasty tasty-quickcheck ];
            homepage = "https://github.com/k0001/instant-aeson";
            description = "Generic Aeson instances through instant-generics";
            license = stdenv.lib.licenses.bsd3;
            src = fetchFromGitHub {
                owner = "k0001";
                repo = "instant-aeson";
                rev = "30128a29130e37935ae79608ea8bb11d07ec1dab";
                sha256 = "15j2yrkw7ir6wqgdggi9y9a1pspmp61r3zfgxn5c2yx7dxhzlmg0";
            };
          }) {};

        instant-bytes = self.callPackage (
          { mkDerivation, base, stdenv, bytes, instant-generics
          , tasty, tasty-quickcheck }:
          mkDerivation {
            pname = "instant-bytes";
            version = "0.1";
            buildDepends = [
              base bytes instant-generics tasty tasty-quickcheck ];
            homepage = "https://github.com/k0001/instant-bytes";
            description = "Generic Serial instances through instant-generics";
            license = stdenv.lib.licenses.bsd3;
            src = fetchFromGitHub {
                owner = "k0001";
                repo = "instant-bytes";
                rev = "f2095946f5879dc8a21efc79c39bb24145ae816c";
                sha256 = "1sbbnln4g8jmcwla8irvrfy21hk82iw99gl01c7fmis2k1v93b15";
            };
          }) {};

        instant-deepseq = self.callPackage (
          { mkDerivation, base, stdenv, deepseq, instant-generics }:
          mkDerivation {
            pname = "instant-deepseq";
            version = "0.1";
            buildDepends = [ base deepseq instant-generics ];
            homepage = "https://github.com/k0001/instant-deepseq";
            description = "Generic NFData instances through instant-generics";
            license = stdenv.lib.licenses.bsd3;
            src = fetchFromGitHub {
                owner = "k0001";
                repo = "instant-deepseq";
                rev = "15a0af31a105f138bcbb6fc76e34b05e38acd9a0";
                sha256 = "1s16xmmvqg3agh18bf65iy4hpv0l3nvyp1snjw5w0yg8ra457nmx";
            };
          }) {};

        instant-hashable = self.callPackage (
          { mkDerivation, base, stdenv, hashable, instant-generics }:
          mkDerivation {
            pname = "instant-hashable";
            version = "0.1";
            buildDepends = [ base hashable instant-generics ];
            homepage = "https://github.com/k0001/instant-hashable";
            description = "Generic Hashable instances through instant-generics";
            license = stdenv.lib.licenses.bsd3;
            src = fetchFromGitHub {
                owner = "k0001";
                repo = "instant-hashable";
                rev = "abdef4021acf1d667dfa1fea1c9f52548501bf8f";
                sha256 = "025j89k0ph10splw9xx5hwzk311nh7pxq317k90sxsx1ijcgc3q5";
            };
          }) {};

        instant-generics = haskell-ng.lib.overrideCabal super.instant-generics (drv: {
          src = fetchFromGitHub {
              owner = "k0001";
              repo = "instant-generics";
              rev = "3f46e7da4667bcdc0ed637c8141ace4330076862";
              sha256 = "0h79xlws17cgcc4zwh9z95wscxa57gy9hc48mz7b7j1vipyzx1i5";
          };
        });

      };
   };

in pkgs.myEnvFun {
     name = "myEnv";
     buildInputs = [
       (hs.ghcWithPackages (p: [
          p.singletons
          p.instant-generics
          p.instant-aeson
          p.instant-bytes
          p.instant-deepseq
          p.instant-hashable
          p.exinst
          p.exinst-aeson
          p.exinst-bytes
          p.exinst-deepseq
          p.exinst-hashable ]))
     ];
   }

