{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.direnv = {
      enable = lib.mkEnableOption "the Direnv feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "direnv" (user: {
      home.packages = with pkgs; [ direnv ];
      programs.emacs = mkElispConfig {
        name = "ordenada-direnv";
        config = ''
          (eval-when-compile (require 'envrc))
          (add-hook 'after-init-hook #'envrc-global-mode)
          (with-eval-after-load 'envrc
            (keymap-set envrc-mode-map "C-c E" #'envrc-command-map))
        '';
        elispPackages = with pkgs.emacsPackages; [ envrc ];
      };
    });
  };
}
