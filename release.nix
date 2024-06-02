let pkgs = import ./nixpkgs/23.11 {};
    ghcs = {
      "ghc8107" = pkgs.haskell.packages.ghc8107;
      "ghc902" = pkgs.haskell.packages.ghc902;
      "ghc928" = pkgs.haskell.packages.ghc928;
      "ghc948" = pkgs.haskell.packages.ghc948;
      "ghc963" = pkgs.haskell.packages.ghc963;
      "ghc981" = (import ./nixpkgs/unstable {}).haskell.packages.ghc981;
    };
in
  pkgs.lib.mapAttrs (_: ghc: ghc.callCabal2nix "monoidal-containers" (builtins.fetchGit ./.) {}) ghcs
