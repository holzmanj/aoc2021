{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./aoc2021.nix {}
