{ lib, pkgs, ... }:

let
  ordenada = lib.makeExtensible (
    final:
    let
      callPackage = lib.callPackageWith (self // { inherit lib pkgs; });
      self = {
        emacs = callPackage ./emacs.nix { };
        home = callPackage ./home.nix { };
        options = callPackage ./options.nix { };
      };
    in
    self
  );
in
ordenada.extend (final: prev: lib.foldr (a: b: a // b) { } (lib.attrValues prev))
