{ mkDerivation, aeson, base, containers, deepseq, hashable, lens
, newtype, semialign ? null, semigroups, stdenv, these
, unordered-containers
}:
mkDerivation {
  pname = "monoidal-containers";
  version = "0.5.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers deepseq hashable lens newtype
    semigroups these unordered-containers
  ] ++ stdenv.lib.optional (semialign == null) [ semialign ];
  configureFlags =
    stdenv.lib.optional (semialign == null) [ "-f-split-these" ];
  homepage = "http://github.com/bgamari/monoidal-containers";
  description = "Containers with monoidal accumulation";
  license = stdenv.lib.licenses.bsd3;
}
