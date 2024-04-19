#!/usr/bin/env bash
set -eu

nix eval --impure --raw '.#options.optionsJSON'
