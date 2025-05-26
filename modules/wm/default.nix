{ config, lib, pkgs, ... }:

let
  inherit (lib) types mkOption mkEnableOption;
in
{
  imports = [
    ./bar/waybar.nix
    ./launcher/bemenu.nix
    ./kanshi.nix
    ./sway.nix
    ./swaylock.nix
    ./swaync.nix
    ./wlogout.nix
  ];
  options = {
    ordenada.globals.wayland = mkOption {
      type = types.nullOr types.bool;
      description = "Whether or not the WM is running under wayland.";
      default = false;
    };
    ordenada.globals.wm = mkOption {
      type = types.nullOr types.str;
      description = "The system wide used window manager.";
      default = null;
    };
    ordenada.globals.launcher = mkOption {
      type = types.nullOr types.str;
      description = "The system wide used application launcher.";
      default = null;
    };
    ordenada.globals.bar = mkOption {
      type = types.nullOr types.str;
      description = "The system wide used bar.";
      default = null;
    };
  };
}
