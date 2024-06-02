{ ghc ? "ghc981" # see release.nix for options
}:
(import ./release.nix).${ghc}.env
