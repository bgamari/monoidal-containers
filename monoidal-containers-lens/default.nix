{ mkDerivation, base, containers, hashable, lens
, stdenv, unordered-containers, monoidal-containers
}:
mkDerivation {
  pname = "monoidal-containers-lens";
  version = "0.6";
  src = ./.;
  libraryHaskellDepends = [
    base containers hashable lens unordered-containers monoidal-containers
  ];
  homepage = "http://github.com/bgamari/monoidal-containers";
  description = "Lens instances for monoidal-containers";
  license = stdenv.lib.licenses.bsd3;
}
