{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.tramp = {
      enable = lib.mkEnableOption "Emacs TRAMP feature.";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.tramp" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-tramp";
        config = ''
          (with-eval-after-load 'tramp
            (setopt tramp-verbose 1)
            (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
        '';
      };
    });
  };
}
