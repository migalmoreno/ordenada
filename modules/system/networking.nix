{ mkFeature, ... }:

mkFeature {
  name = "networking";
  nixos = {
    networking.useDHCP = false;
    networking.networkmanager.enable = true;
    ordenada.features.userInfo.extraGroups = [ "networkmanager" ];
  };
}
