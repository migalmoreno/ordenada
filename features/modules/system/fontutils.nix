{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  inherit (lib) mkOption mkPackageOption types;
in
mkFeature {
  name = "fontutils";
  options =
    { config, pkgs, ... }:
    let
      fontModule = types.submodule {
        options = {
          name = mkOption {
            description = "The name of the font.";
            type = types.str;
          };
          package = mkPackageOption pkgs "font" { default = null; };
          size = mkOption {
            description = "The size of the font.";
            type = types.int;
          };
        };
      };
    in
    {
      fonts = {
        monospace = mkOption {
          type = fontModule;
          description = "The monospace font to use.";
          default = {
            name = "Iosevka";
            package = pkgs.iosevka;
            size = 11;
          };
        };
        serif = mkOption {
          type = fontModule;
          description = "The serif font to use.";
          default = {
            name = "IBM Plex Sans";
            package = pkgs.ibm-plex;
            size = 11;
          };
        };
        sans = mkOption {
          type = fontModule;
          description = "The sans serif font to use.";
          default = config.ordenada.features.fontutils.fonts.serif;
        };
        unicode = mkOption {
          type = fontModule;
          description = "The unicode font to use.";
          default = {
            name = "Noto Color Emoji";
            package = pkgs.noto-fonts-emoji;
            size = 11;
          };
        };
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      fonts.fontconfig = {
        enable = true;
        defaultFonts = with config.ordenada.features.fontutils.fonts; {
          sansSerif = [ sans.name ];
          serif = [ serif.name ];
          monospace = [ monospace.name ];
          emoji = [ unicode.name ];
        };
      };
      home.packages =
        with pkgs;
        with config.ordenada.features.fontutils.fonts;
        [
          monospace.package
          sans.package
          serif.package
          unicode.package
          dejavu_fonts
          unifont
          font-awesome
        ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-fontutils";
        config = with config.ordenada.features.fontutils.fonts; ''
          (with-eval-after-load 'fontset
            (set-fontset-font t 'symbol "Unifont" nil 'append)
            (set-fontset-font t 'unicode "Unifont" nil 'append)
            (set-fontset-font "fontset-default" nil (font-spec :name "Unifont")))

          (setq use-default-font-for-symbols nil)

          (require 'fontaine)
          (setq fontaine-presets
                '((t :default-family "${monospace.name}"
                    :default-height ${toString (monospace.size * 10 - 5)}
                    :fixed-pitch-family "${monospace.name}"
                    :fixed-pitch-height 1.0
                    :variable-pitch-family "${sans.name}"
                    :variable-pitch-height 1.0
                    :variable-pitch-weight regular)))

          (require 'xdg)
          (setq fontaine-latest-state-file
                (expand-file-name "emacs/fontaine-latest.state.eld"
                                  (or (xdg-cache-home) "~/.cache")))

          (defun ordenada-font--set-default-fonts ()
            (fontaine-set-preset t))

          (if after-init-time
            (when (display-graphic-p) (ordenada-font--set-default-fonts))
            (add-hook 'after-init-hook #'ordenada-font--set-default-fonts))

          (add-hook 'modus-themes-after-load-theme-hook #'fontaine-apply-current-preset)
        '';
        elispPackages = with pkgs.emacsPackages; [ fontaine ];
      };
    };
}
