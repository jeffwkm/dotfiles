{ lib, ... }:
let inherit (lib) recursiveUpdate foldl' head tail;
in { merge = xs: foldl' (a: b: recursiveUpdate a b) (head xs) (tail xs); }
