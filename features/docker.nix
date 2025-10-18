{ ordenada-lib, lib, ... }:

rec {
  options.ordenada.features.docker = {
    enable = lib.mkEnableOption "the Docker feature";
    key = lib.mkOption {
      type = lib.types.str;
      default = "D";
      description = "Keybinding to launch Emacs Docker interface.";
    };
  };
  config.ordenada.modules = ordenada-lib.mkFeature "docker" {
    inherit options;
    nixos = {
      virtualisation.docker.enable = true;
    };
    homeManager =
      { config, pkgs, ... }:
      {
        programs.emacs = ordenada-lib.mkElispConfig {
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
  };
}
