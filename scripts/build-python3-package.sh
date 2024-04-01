#!/usr/bin/env sh
set -eu

file="$1"

nix-build -E "with import <nixpkgs> {}; pkgs.python3Packages.callPackage $file {}"
