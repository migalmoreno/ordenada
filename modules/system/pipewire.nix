{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.pipewire;
in
{
  options = {
    ordenada.features.pipewire = {
      enable = lib.mkEnableOption "the Pipewire feature";
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        pulse.enable = true;
      };
    })
    {
      home-manager = mkHomeConfig config "pipewire" (user: {
        home.packages = [ pkgs.pulseaudio ];
        home.sessionVariables = {
          RTC_USE_PIPEWIRE = "true";
        };
        programs.emacs = mkElispConfig {
          name = "ordenada-pulseaudio-control";
          config = ''
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "v" #'pulseaudio-control-map))
            (with-eval-after-load 'pulseaudio-control
              (keymap-set pulseaudio-control-map "L"
                #'pulseaudio-control-toggle-sink-input-mute-by-index)
              (setopt pulseaudio-control-volume-step "5%")
              (setopt pulseaudio-control-volume-verbose nil)
              (pulseaudio-control-default-sink-mode)
              (pulseaudio-control-default-source-mode))
          '';
          elispPackages = with pkgs.emacsPackages; [ pulseaudio-control ];
        };
      });
    }
  ];
}
