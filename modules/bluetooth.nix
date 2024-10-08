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
              (define-key ordenada-app-map (kbd "B") #'bluetooth-list-devices))
            (with-eval-after-load 'bluetooth
              (define-key bluetooth-mode-map "C" #'bluetooth-connect-profile))
          '';
          elispPackages = with pkgs.emacsPackages; [ bluetooth ];
        };
      });
    }
  ];
}
