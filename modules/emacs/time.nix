{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption mkEnableOption types;
in
{
  options = {
    ordenada.features.emacs.time = {
      enable = lib.mkEnableOption "the Emacs time feature";
      worldClockKey = mkOption {
        type = types.str;
        description = "Keybinding for Emacs daemons map operations.";
        default = "C";
      };
      worldClockTimezones = mkOption {
        type = types.attrsOf types.str;
        description = "Attribute set of timezones and places for world clock to display.";
        default = { };
      };
      worldClockTimeFormat = mkOption {
        type = types.str;
        description = "Time format for world clock.";
        default = "%A %d %B %R %Z";
      };
      displayDate = mkEnableOption "displaying day and date as well as time for display-time";
      display24hr = mkEnableOption "displaying time in hh:mm format";
      displayTime = mkEnableOption "Display Time Mode to display time in the mode line";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.time" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-time";
        config = with user.features.emacs.time; ''
          (eval-when-compile
           (require 'time))
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "${worldClockKey}" #'world-clock))
          ${mkIf (worldClockTimezones != { }) ''
            (setopt world-clock-list ${mkAlist worldClockTimezones})
          ''}
          (setopt display-time-world-time-format "${worldClockTimeFormat}")
          (setopt display-time-default-load-average nil)
          (setopt display-time-load-average-threshold 0)
          (setopt display-time-day-and-date ${mkBoolean displayDate})
          ${mkIf displayTime "(display-time-mode 1)"}
        '';
      };
    });
  };
}
