{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "android";
  options.fdroidKey = lib.mkOption {
    type = lib.types.str;
    description = "Keybinding for fdroid.el map operations.";
    default = "f";
  };
  nixos =
    { pkgs, ... }:
    {
      programs.adb.enable = true;
      virtualisation.waydroid.enable = true;
      environment.systemPackages = [ pkgs.wl-clipboard ];
      ordenada.features.userInfo.extraGroups = [ "adbusers" ];
    };
  homeManager =
    { config, pkgs, ... }:
    let
      emacs-fdroid = pkgs.emacsPackages.melpaBuild {
        pname = "fdroid";
        version = "0.1.1";
        src = pkgs.fetchFromGitHub {
          owner = "migalmoreno";
          repo = "fdroid.el";
          rev = "94c2011630886011e15d37c163f7f59f2a045db0";
          hash = "sha256-6bSxjZOAZ8PpiaqbKdE2elobGgwmt1B8KZ7I9f+mhh0=";
        };
        packageRequires = with pkgs.emacsPackages; [ embark ];
      };
    in
    {
      home.packages = with pkgs; [
        android-tools
        payload-dumper-go
        fdroidcl
      ];
      wayland.windowManager.sway.config.floating.criteria = [ { app_id = "Waydroid"; } ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-android";
        config = # elisp
          ''
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${config.ordenada.features.android.fdroidKey}" 'fdroid-map))
          '';
        elispPackages = [
          emacs-fdroid
        ];
      };
    };
}
