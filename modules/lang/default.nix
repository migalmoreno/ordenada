{ config, lib, pkgs, ... }:

{
  imports = [
    ./clojure.nix
    ./javascript.nix
    ./markdown.nix
    ./nix.nix
    ./yaml.nix
  ];
}