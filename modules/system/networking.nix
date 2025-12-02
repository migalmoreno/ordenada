{ lib, mkFeature, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
in
mkFeature {
  name = "networking";
  options = {
    ordenada.features.networking = {
      enable = mkEnableOption "the networking feature";
      nameservers = mkOption {
        description = "List of nameservers to use.";
        type = types.listOf types.str;
        default = [ ];
      };
    };
  };
  nixos = {config, ...}: with config.ordenada.features.networking; {
    networking.useDHCP = false;
    networking.networkmanager.enable = true;
    networking.nameservers = nameservers;
    ordenada.features.userInfo.extraGroups = [ "networkmanager" ];
    programs.nm-applet = {
      enable = true;
      indicator = true;
    };
  };
}
