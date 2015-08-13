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

        instant-aeson =
          let src = fetchFromGitHub {
                       owner = "k0001";
                       repo = "instant-aeson";
                       rev = "d49ba0d7e5477e1cac84bd381ee9a3e6ebe101c3";
                       sha256 = "187n8vmb8cq9jdanb0fd5c92p5yk7l6f29aqx0l823qh8rsh047v";
                    };
          in self.callPackage "${src}/nix/default.nix" {};
  
        instant-bytes = 
          let src = fetchFromGitHub {
                       owner = "k0001";
                       repo = "instant-bytes";
                       rev = "7fffcba9d1f4985f3fa8819bbbd6f8c774a66db8";
                       sha256 = "1r6b4d0b0lqmdn349q738nnddb5ja3yl0ivjibjybh4icg44x30k";
                    };
          in self.callPackage "${src}/nix/default.nix" {};
  
        instant-deepseq = 
          let src = fetchFromGitHub {
                       owner = "k0001";
                       repo = "instant-deepseq";
                       rev = "d0cd3340e51574031a6404ea06d31bd917610dc9";
                       sha256 = "132gi12m65vpr3fcbcphz69qlcs875c8i613ismf1s1p2ml9cibr";
                    };
          in self.callPackage "${src}/nix/default.nix" {};
  
        instant-hashable = 
          let src = fetchFromGitHub {
                       owner = "k0001";
                       repo = "instant-hashable";
                       rev = "53446751fd0cdfe757930c8a322f98c077c96945";
                       sha256 = "0gqsmnm0g4g5j6k6bfn2ykiqv5d4v5dq6jnhbq1kmy99hg27rc0q";
                    };
          in self.callPackage "${src}/nix/default.nix" {};
  
  

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

