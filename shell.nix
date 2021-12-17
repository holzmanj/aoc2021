{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.shellFor {
  packages = p: [
    (import ./default.nix {})
  ];
  buildInputs = [
    pkgs.cabal-install
  ];
  shellHook = ''
    if [ -f .env ]; then
      set -a
      source .env
      set +a
    else
      echo "Please add a .env file with the following line:"
      echo "    AOC_SESSION=<session id here>"
    fi
  '';
}
