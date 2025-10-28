{ mkFeature, ... }:

mkFeature {
  name = "qmk";
  nixos =
    { pkgs, ... }:
    {
      hardware.keyboard.qmk.enable = true;
      services.udev.packages = with pkgs; [
        via
        vial
      ];
      environment.systemPackages = with pkgs; [
        via
        vial
      ];
    };
}
