{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "transmission";
  options = {
    host = lib.mkOption {
      type = lib.types.str;
      description = "Hostname, IP address or socket address of Transmission session.";
      default = "localhost";
    };
    key = lib.mkOption {
      type = lib.types.str;
      description = "Keybinding to launch transmission.el.";
      default = "T";
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-transmission";
        config =
          with config.ordenada.features.transmission; # elisp
          ''
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${key}" #'transmission))

            (with-eval-after-load 'transmission
              (setopt transmission-host "${host}")
              (let ((map transmission-mode-map))
                (keymap-set map "R" #'transmission-move)))
          '';
        elispPackages = with pkgs.emacsPackages; [ transmission ];
      };
    };
}
