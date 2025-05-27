{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) types mkOption mkEnableOption;
in
{
  options = {
    ordenada.features.emacs.which-key = {
      enable = mkEnableOption "the Emacs which-key feature";
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
  };
  config = {
    home-manager = mkHomeConfig config "emacs.which-key" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-which-key";
        config = with user.features.emacs.which-key; ''
          (require 'which-key)
          (setopt which-key-min-display-lines ${toString minHeight})
          (setopt which-key-ellipsis "...")
          (setopt which-key-idle-delay ${toString idleDelay})
          (which-key-mode 1)
          (keymap-global-set "C-h C-k" #'which-key-show-top-level)
        '';
        elispPackages = with pkgs.emacsPackages; [ which-key ];
      };
    });
  };
}
