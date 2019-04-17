{ mkDerivation, aeson, base, bytestring, casing, containers
, directory, exceptions, extra, filepath, hashable, indents
, monad-logger, mtl, optparse-applicative, parsec, pretty, stdenv
, text, time, transformers, unordered-containers, vector, xml
}:
mkDerivation {
  pname = "metagen";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring casing containers directory exceptions extra
    filepath hashable indents monad-logger mtl optparse-applicative
    parsec pretty text time transformers unordered-containers vector
    xml
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
