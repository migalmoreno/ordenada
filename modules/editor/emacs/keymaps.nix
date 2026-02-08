{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "keymaps"
  ];
  options =
    { config, ... }:
    let
      inherit (lib) mkOption types;
    in
    {
      appMapPrefix = mkOption {
        type = types.str;
        description = "The prefix key used for ordenada-app-map.";
        default = "a";
      };
      toggleMapPrefix = lib.mkOption {
        type = types.str;
        description = "The prefix key used for ordenada-toggle-map.";
        default = "t";
      };
      appMap = lib.mkOption {
        type = with types; attrsOf str;
        default = with config.ordenada.features.emacs; {
          calendar = calendar.calendarKey;
          appt = calendar.apptKey;
          daemons = daemons.key;
        };
        description = "The Emacs keymap for application keybindings.";
      };
      toggleMap = mkOption {
        type = types.attrs;
        description = "The Emacs keymap for toggle keybindings.";
        default = { };
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-keymaps";
        config = with config.ordenada.features.emacs.keymaps; ''
          (defvar ordenada-app-map nil "Prefix keymap for applications.")
          (define-prefix-command 'ordenada-app-map nil)
          (defvar ordenada-toggle-map nil
            "Prefix keymap for binding various minor modes for toggling functionality.")
          (define-prefix-command 'ordenada-toggle-map nil)
          (defun ordenada-toggle-line-numbers-mode ()
          "Toggles between absolute and relative line numbers mode."
          (interactive)
            (if (or (eq display-line-numbers 'absolute) (eq display-line-numbers t))
             (menu-bar--display-line-numbers-mode-relative)
             (menu-bar--display-line-numbers-mode-absolute)))

          (define-key mode-specific-map (kbd "${appMapPrefix}") '("applications" . ordenada-app-map))
          (define-key mode-specific-map (kbd "${toggleMapPrefix}") '("toggles" . ordenada-toggle-map))

          (keymap-set ordenada-toggle-map "f" '("Toggle fill column indicator" . display-fill-column-indicator-mode))
          (keymap-set ordenada-toggle-map "n" '("Toggle line numbers" . display-line-numbers-mode))
          (keymap-set ordenada-toggle-map "N" '("Toggle relative / absolute line numbers" . ordenada-toggle-line-numbers-mode))
        '';
      };
    };
}
