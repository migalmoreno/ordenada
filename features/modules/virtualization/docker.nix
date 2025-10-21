{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "docker";
  nixos.virtualisation.docker.enable = true;
  homeManager =
    { config, pkgs, ... }:
    {
      options.ordenada.features.docker.key = lib.mkOption {
        type = lib.types.str;
        default = "D";
        description = "Keybinding to launch Emacs Docker interface.";
      };
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-docker";
        config = ''
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "${config.ordenada.features.docker.key}" #'docker))
          (add-to-list 'auto-mode-alist '(".*Dockerfile\\'" . dockerfile-mode))
        '';
        elispPackages = with pkgs.emacsPackages; [
          docker
          dockerfile-mode
        ];
      };
    };
}
