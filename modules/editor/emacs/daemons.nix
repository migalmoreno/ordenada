{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "daemons"
  ];
  options = with lib; {
    fillFrame = mkEnableOption "showing the list of daemons in the current full frame";
    key = mkOption {
      type = types.str;
      description = "Keybinding for Emacs daemons map operations.";
      default = "d";
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-daemons";
        config = with config.ordenada.features.emacs; ''
          (defvar ordenada-daemons-map nil
            "Map to bind `appt' commands under.")
          (define-prefix-command 'ordenada-daemons-map)
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "${keymaps.appMap.daemons}" #'ordenada-daemons-map)
            (let ((map ordenada-daemons-map))
              (keymap-set map "r" #'daemons-restart)
              (keymap-set map "t" #'daemons-status)
              (keymap-set map "s" #'daemons-start)
              (keymap-set map "e" #'daemons-enable)
              (keymap-set map "p" #'daemons-stop)
              (keymap-set map "d" #'daemons-disable)
              (keymap-set map "l" #'daemons)))
          (add-hook 'daemons-mode-hook #'eldoc-mode)
          (with-eval-after-load 'daemons
            (setopt daemons-list-fill-frame ${ordenada-lib.elisp.toBoolean daemons.fillFrame})
            (setopt daemons-show-output-in-minibuffer t)
            (setopt daemons-systemd-is-user t))
        '';
        elispPackages = with pkgs.emacsPackages; [ daemons ];
      };
    };
}
