{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.keymaps = {
      enable = mkEnableTrueOption "the Emacs keymaps feature";
      appMapPrefix = lib.mkOption {
        type = lib.types.str;
        description = "The prefix key used for ordenada-app-map.";
        default = "a";
      };
      toggleMapPrefix = lib.mkOption {
        type = lib.types.str;
        description = "The prefix key used for ordenada-toggle-map.";
        default = "t";
      };
      appMap = lib.mkOption {
        type = lib.types.attrs;
        default = with config.ordenada.features.emacs; {
          calendar = calendar.calendarKey;
          appt = calendar.apptKey;
        };
        description = "The Emacs keymap for application keybindings.";
      };
      toggleMap = lib.mkOption {
        type = lib.types.attrs;
        description = "The Emacs keymap for toggle keybindings.";
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.keymaps" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-keymaps";
        config = with user.features.emacs.keymaps; ''
          (defvar ordenada-app-map nil "Prefix keymap for applications.")
          (define-prefix-command 'ordenada-app-map nil)
          (defvar ordenada-toggle-map nil
            "Prefix keymap for binding various minor modes for toggling functionality.")
          (define-prefix-command 'ordenada-toggle-map nil)
          (define-key mode-specific-map (kbd "${appMapPrefix}") '("applications" . ordenada-app-map))
          (define-key mode-specific-map (kbd "${toggleMapPrefix}") '("toggles" . ordenada-toggle-map))
          (keymap-set ordenada-toggle-map "f" #'display-fill-column-indicator-mode)
        '';
      };
    });
  };
}
