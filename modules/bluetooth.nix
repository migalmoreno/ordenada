{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.bluetooth;
in
{
  options = {
    ordenada.features.bluetooth = {
      enable = lib.mkEnableOption "the Bluetooth feature";
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      hardware.bluetooth.enable = true;
      hardware.bluetooth.powerOnBoot = true;
    })
    {
      home-manager = mkHomeConfig config "bluetooth" (user: {
        programs.emacs = pkgs.lib.ordenada.mkElispConfig {
          name = "ordenada-bluetooth";
          config = ''
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "B" #'bluetooth-list-devices))
            (with-eval-after-load 'bluetooth
              (keymap-set bluetooth-mode-map "C" #'bluetooth-connect-profile)
              (setopt bluetooth-battery-display-warning nil))

            (advice-add 'bluetooth-pa--authorize-service :override #'ignore)
          '';
          elispPackages = with pkgs.emacsPackages; [ bluetooth ];
        };
      });
    }
  ];
}
