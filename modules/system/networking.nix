{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.networking = {
      enable = lib.mkEnableOption "the networking feature";
    };
  };
  config = lib.mkIf config.ordenada.features.networking.enable {
    networking.useDHCP = false;
    networking.networkmanager.enable = true;
    users = mkHomeConfig config "networking" (user: {
      extraGroups = [ "networkmanager" ];
    });
  };
}
