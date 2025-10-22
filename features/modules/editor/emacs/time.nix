{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "time"
  ];
  options = with lib; {
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
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-time";
        config =
          with config.ordenada.features.emacs.time;
          with ordenada-lib.elisp;
          ''
            (eval-when-compile
             (require 'time))
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${worldClockKey}" #'world-clock))
            ${lib.optionalString (worldClockTimezones != { }) ''
              (setopt world-clock-list ${toAlist worldClockTimezones})
            ''}
            (setopt display-time-world-time-format "${worldClockTimeFormat}")
            (setopt display-time-default-load-average nil)
            (setopt display-time-load-average-threshold 0)
            (setopt display-time-day-and-date ${toBoolean displayDate})
            ${lib.optionalString displayTime "(display-time-mode 1)"}
          '';
      };
    };
}
