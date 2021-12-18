{ mkDerivation, base, bytestring, containers, directory, hpack
, http-client, http-client-tls, http-types, lib, split
}:
mkDerivation {
  pname = "aoc2021";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers split ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring containers directory http-client http-client-tls
    http-types split
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
