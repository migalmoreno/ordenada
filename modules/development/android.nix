{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.android;
in
{
  options = {
    ordenada.features.android = {
      enable = lib.mkEnableOption "the Android feature";
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.adb.enable = true;
      services.udev.packages = [ pkgs.android-udev-rules ];
      virtualisation.waydroid.enable = true;
      environment.systemPackages = [ pkgs.wl-clipboard ];
    })
    {
      users = mkHomeConfig config "android" (user: {
        extraGroups = [ "adbusers" ];
      });
      home-manager = mkHomeConfig config "android" (user: {
        home.packages = with pkgs; [
          android-tools
          payload-dumper-go
          fdroidcl
        ];
        wayland.windowManager.sway.config.floating.criteria = [ { app_id = "Waydroid"; } ];
      });
    }
  ];
}
