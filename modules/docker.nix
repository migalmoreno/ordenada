{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkEnableOption mkOption types;
  cfg = config.ordenada.features.docker;
in
{
  options = {
    ordenada.features.docker = {
      enable = mkEnableOption "the Docker feature";
      key = mkOption {
        type = types.str;
        default = "D";
        description = "Keybinding to launch Emacs Docker interface.";
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable { virtualisation.docker.enable = true; })
    {
      users = mkHomeConfig config "docker" (user: {
        extraGroups = [ "docker" ];
      });
      home-manager = mkHomeConfig config "docker" (user: {
        programs.emacs = mkElispConfig {
          name = "ordenada-docker";
          config = ''
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${user.features.docker.key}" #'docker))
            (add-to-list 'auto-mode-alist '(".*Dockerfile\\'" . dockerfile-mode))
          '';
          elispPackages = with pkgs.emacsPackages; [
            docker
            dockerfile-mode
          ];
        };
      });
    }
  ];
}
