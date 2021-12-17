{ mkDerivation, base, directory, hpack, http-client
, http-client-tls, lib
}:
mkDerivation {
  pname = "AOC2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base directory http-client http-client-tls
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
