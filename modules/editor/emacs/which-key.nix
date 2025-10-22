{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "which-key"
  ];
  options = with lib; {
    minHeight = mkOption {
      type = types.number;
      default = 1;
      description = "The minimum height of the which-key popup.";
    };
    idleDelay = mkOption {
      type = types.number;
      default = 1;
      description = "The number of seconds to wait for the which-key to show up.";
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-which-key";
        config = with config.ordenada.features.emacs.which-key; ''
          (require 'which-key)
          (setopt which-key-min-display-lines ${toString minHeight})
          (setopt which-key-ellipsis "...")
          (setopt which-key-idle-delay ${toString idleDelay})
          (which-key-mode 1)
          (keymap-global-set "C-h C-k" #'which-key-show-top-level)
        '';
        elispPackages = with pkgs.emacsPackages; [ which-key ];
      };
    };
}
