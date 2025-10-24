{ mkFeature, ... }:

mkFeature {
  name = "android";
  nixos =
    { pkgs, ... }:
    {
      programs.adb.enable = true;
      services.udev.packages = [ pkgs.android-udev-rules ];
      virtualisation.waydroid.enable = true;
      environment.systemPackages = [ pkgs.wl-clipboard ];
      ordenada.features.userInfo.extraGroups = [ "adbusers" ];
    };
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        android-tools
        payload-dumper-go
        fdroidcl
      ];
      wayland.windowManager.sway.config.floating.criteria = [ { app_id = "Waydroid"; } ];
    };
}
