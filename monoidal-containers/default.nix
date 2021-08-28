{ mkDerivation, aeson, base, containers, deepseq, hashable, lens
, newtype, semialign, semigroups, stdenv, these
, unordered-containers
}:
mkDerivation {
  pname = "monoidal-containers";
  version = "0.6";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers deepseq hashable lens newtype semialign
    semigroups these unordered-containers
  ];
  homepage = "http://github.com/bgamari/monoidal-containers";
  description = "Containers with monoidal accumulation";
  license = stdenv.lib.licenses.bsd3;
}
