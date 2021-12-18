{ mkDerivation, base, bytestring, directory, hpack, http-client
, http-client-tls, http-types, lib, split
}:
mkDerivation {
  pname = "AOC2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring directory http-client http-client-tls http-types
    split
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
