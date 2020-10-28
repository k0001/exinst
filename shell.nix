let pkgs = import ./nix;
in { ghc865 = pkgs._here.ghc865._shell;
     ghc8101 = pkgs._here.ghc865._shell; }
