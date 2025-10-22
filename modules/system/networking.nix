{ mkFeature, ... }:

mkFeature {
  name = "networking";
  nixos = {
    networking.useDHCP = false;
    networking.networkmanager.enable = true;
  };
}
