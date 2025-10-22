{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "ebdb"
  ];
  options =
    { config, ... }:
    {
      sources = lib.mkOption {
        type = with lib; types.listOf types.str;
        description = "The list of EBDB database sources.";
        default = [ "${config.ordenada.features.xdg.userDirs.documents}/contacts" ];
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-ebdb";
        config = ''
          (defvar ordenada-ebdb-map nil
            "Map to bind EBDB commands to.")
          (define-prefix-command 'ordenada-ebdb-map)
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "b" #'ordenada-ebdb-map)
            (let ((map ordenada-ebdb-map))
              (keymap-set map "a" #'ebdb-display-all-records)
              (keymap-set map "c" #'ebdb-create-record-extended)))

          (with-eval-after-load 'ebdb
            (require 'ebdb-i18n)
            (require 'ebdb-vcard)
            (require 'ebdb-org)
            (require 'ebdb-mua)
            (with-eval-after-load 'ebdb-mua
              (setopt ebdb-mua-pop-up nil))
            (require 'ebdb-notmuch)
            (require 'ebdb-message)
            (require 'ebdb-ispell)
            (require 'ebdb-gnus)
            (setq ebdb-sources ${ordenada-lib.elisp.toList config.ordenada.features.emacs.ebdb.sources})
            (setq ebdb-default-country nil)
            (setq ebdb-default-window-size 0.4)
            (setq ebdb-dedicated-window 'ebdb)
            (setq ebdb-mail-avoid-redundancy t)
            (setq ebdb-complete-mail 'capf)
            (setq ebdb-completion-display-record nil)
            (setq ebdb-complete-mail-allow-cycling nil)
            (setq ebdb-save-on-exit t)
            (keymap-set ebdb-mode-map "q" #'kill-current-buffer))
        '';
        elispPackages = with pkgs.emacsPackages; [ ebdb ];
      };
    };
}
