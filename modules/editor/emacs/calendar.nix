{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "calendar"
  ];
  options =
    { config, ... }:
    let
      inherit (lib) mkEnableOption mkOption types;
    in
    {
      dateStyle = mkOption {
        type = types.enum [
          "american"
          "european"
          "iso"
        ];
        description = "The style for displaying dates in the Emacs calendar and diary.";
        default = "iso";
      };
      diaryFile = mkOption {
        type = types.str;
        description = "The name of the file where the Emacs diary is located.";
        default = "${config.ordenada.features.xdg.userDirs.documents}/diary";
      };
      weekNumbers = mkEnableOption "showing week numbers in the Emacs calendar";
      calendarKey = mkOption {
        type = types.str;
        description = "Keybinding to launch the Emacs Calendar.";
        default = "c";
      };
      apptKey = mkOption {
        type = types.str;
        description = "Keybinding to launch the Emacs appointment notifications.";
        default = "A";
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-calendar";
        config = with config.ordenada.features.emacs; ''
          (defvar ordenada-calendar-appt-map nil
            "Map to bind `appt' commands under.")
          (define-prefix-command 'ordenada-calendar-appt-map)
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "${keymaps.appMap.calendar}" #'calendar))
          (with-eval-after-load 'calendar
            ${lib.optionalString calendar.enable "(require 'ebdb)"}
            (setopt diary-file "${calendar.diaryFile}")
            (setopt calendar-week-start-day 1)
            (setopt calendar-view-diary-initially-flag t)
            (setopt calendar-date-style '${calendar.dateStyle})
            (setopt calendar-mark-diary-entries-flag t)
            ${lib.optionalString calendar.weekNumbers ''
              (setopt calendar-intermonth-header
                      (propertize "WK" 'font-lock-face
                                  'font-lock-function-name-face))
              (setopt calendar-intermonth-text
                      '(propertize
                        (format "%2d"
                                (car
                                 (calendar-iso-from-absolute
                                  (calendar-absolute-from-gregorian
                                   (list month day year)))))
                        'font-lock-face 'font-lock-function-name-face))
            ''})
            (appt-activate 1)
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${keymaps.appMap.appt}" #'ordenada-calendar-appt-map)
              (keymap-set ordenada-calendar-appt-map "a" #'appt-add)
              (keymap-set ordenada-calendar-appt-map "d" #'appt-delete))
            (with-eval-after-load 'appt
              (setopt appt-display-format 'echo)
              (setopt appt-audible nil)
              (setopt appt-message-warning-time 10)
              (setopt appt-display-interval 2)
              (setopt appt-display-diary nil))
        '';
      };
    };
}
