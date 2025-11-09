{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  getDeuteranopia =
    config: if config.ordenada.features.emacs.modus-themes.deuteranopia then "-deuteranopia" else "";
  getLightTheme = config: "modus-operandi${getDeuteranopia config}";
  getDarkTheme = config: "modus-vivendi${getDeuteranopia config}";
  getTheme =
    config:
    if config.ordenada.features.emacs.modus-themes.dark then
      getDarkTheme config
    else
      getLightTheme config;
in
mkFeature {
  name = [
    "emacs"
    "modus-themes"
  ];
  options =
    { config, ... }:
    {
      dark = lib.mkOption {
        type = lib.types.bool;
        description = "Whether to enable dark mode in Modus Themes.";
        default = config.ordenada.features.theme.polarity == "dark";
      };
      deuteranopia = lib.mkEnableOption "deuteranopia support in Modus Themes";
    };
  nixos =
    { config, ... }:
    {
      ordenada.features.emacs.defaultThemes = {
        light = getLightTheme config;
        dark = getDarkTheme config;
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-modus-themes";
        config =
          with config.ordenada.features.emacs.appearance; # elisp
          ''
            (eval-when-compile
              (require 'modus-themes)
              (require 'cl-seq))
            (require '${getTheme config}-theme)
            (eval-when-compile
              (enable-theme '${getTheme config}))
            (defgroup ordenada-modus-themes nil
              "Configuration related to `modus-themes'."
              :group 'ordenada)

            (defcustom ordenada-modus-themes-mode-line-padding 1
              "The padding of the mode line."
              :type 'number
              :group 'ordenada-modus-themes)
            (defcustom ordenada-modus-themes-tab-bar-padding 1
              "The padding of the tab bar."
              :type 'number
              :group 'ordenada-modus-themes)
            (defcustom ordenada-modus-themes-header-line-padding 1
              "The padding of the header line."
              :type 'number
              :group 'ordenada-modus-themes)

            (defcustom ordenada-modus-themes-after-enable-theme-hook nil
              "Normal hook run after enabling a theme."
              :type 'hook
              :group 'ordenada-modus-themes)

            (defun ordenada-modus-themes-run-after-enable-theme-hook (&rest _args)
              "Run `ordenada-modus-themes-after-enable-theme-hook'."
              (run-hooks 'ordenada-modus-themes-after-enable-theme-hook))

            (defun ordenada-modus-themes--dark-theme-p (&optional theme)
              "Indicate if there is a curently-active dark THEME."
              (if theme
                  (eq theme '${getLightTheme config})
                  (eq (car custom-enabled-themes) '${getDarkTheme config})))

            (defun ordenada-modus-themes-set-custom-faces (&optional _theme)
              "set faces based on the current theme."
              (interactive)
              (when (modus-themes--current-theme)
                (modus-themes-with-colors
                  (custom-set-faces
                   `(window-divider ((,c :foreground ,bg-main)))
                   `(window-divider-first-pixel ((,c :foreground ,bg-main)))
                   `(window-divider-last-pixel ((,c :foreground ,bg-main)))
                   `(vertical-border ((,c :foreground ,bg-main)))
                   `(tab-bar
                     ((,c :background ,bg-dim
                          :box (:line-width ,ordenada-modus-themes-tab-bar-padding
            				:color ,bg-dim))))
                   `(mode-line
                     ((,c :box (:line-width ,ordenada-modus-themes-mode-line-padding
            				:color ,bg-mode-line-active))))
                   `(mode-line-inactive
                     ((,c :box (:line-width ,ordenada-modus-themes-mode-line-padding
            				:color ,bg-mode-line-inactive))))
                   `(header-line
                     ((,c :box (:line-width ,ordenada-modus-themes-header-line-padding
            				:color ,bg-dim))))
                   `(git-gutter-fr:added
                     ((,c :foreground ,bg-added-fringe :background ,bg-main)))
                   `(git-gutter-fr:deleted
                     ((,c :foreground ,bg-removed-fringe :background ,bg-main)))
                   `(git-gutter-fr:modified
                     ((,c :foreground ,bg-changed-fringe :background ,bg-main)))
                   `(aw-leading-char-face
                     ((,c :height 1.0 :foreground ,blue-cooler)))))))
            ${lib.optionalString config.ordenada.features.emacs.appearance.enable ''
              (setopt ordenada-modus-themes-header-line-padding ${toString headerLinePadding})
              (setopt ordenada-modus-themes-tab-bar-padding ${toString tabBarPadding})
              (setopt ordenada-modus-themes-mode-line-padding ${toString modeLinePadding})
            ''}
            (advice-add 'enable-theme
                        :after #'ordenada-modus-themes-run-after-enable-theme-hook)
            (add-hook 'ordenada-modus-themes-after-enable-theme-hook #'ordenada-modus-themes-set-custom-faces)
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-toggle-map "t" #'modus-themes-toggle))
            (with-eval-after-load 'modus-themes
              (setq modus-themes-common-palette-overrides
                    '((border-mode-line-active unspecified)
                      (border-mode-line-inactive unspecified)
                      (fringe unspecified)
                      (fg-line-number-inactive "gray50")
                      (fg-line-number-active fg-main)
                      (bg-line-number-inactive unspecified)
                      (bg-line-number-active unspecified)
                      (bg-region bg-ochre)
                      (fg-region unspecified)))
                (setq modus-themes-to-toggle '(${getLightTheme config} ${getDarkTheme config}))
                (setq modus-themes-italic-constructs t)
                (setq modus-themes-bold-constructs t)
                (setq modus-themes-mixed-fonts t)
                (setq modus-themes-org-blocks 'gray-background)
                (setq modus-themes-headings (quote ((1 . (1.15))
                                                    (2 . (1.1))
                                                    (3 . (1.1))
                                                    (4 . (1.0))
                                                    (5 . (1.0))
                                                    (6 . (1.0))
                                                    (7 . (0.9))
                                                    (8 . (0.9))))))
            (load-theme '${getTheme config} t)
          '';
        elispPackages = with pkgs.emacsPackages.elpaPackages; [
          modus-themes
        ];
      };
    };
}
