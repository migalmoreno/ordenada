{ config, lib, pkgs, ... }:

let
  inherit (lib) types mkOption mkEnableOption;
in
{
  imports = [
    ./bash.nix
  ];
  options = {
    ordenada.globals.shell = mkOption {
      type = types.nullOr types.str;
      description = "The system wide used shell.";
      default = null;
    };
  };
}
