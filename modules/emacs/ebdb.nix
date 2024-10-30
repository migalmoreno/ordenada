{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.ebdb = {
      enable = lib.mkEnableOption "the Emacs EBDB feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.ebdb" (user: {
      programs.emacs = mkElispConfig {
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
            (setq ebdb-sources (list "~/documents/contacts"))
            (setopt ebdb-default-country nil)
            (setopt ebdb-default-window-size 0.4)
            (setopt ebdb-dedicated-window 'ebdb)
            (setopt ebdb-mail-avoid-redundancy t)
            (setopt ebdb-complete-mail 'capf)
            (setopt ebdb-completion-display-record nil)
            (setopt ebdb-complete-mail-allow-cycling nil)
            (setopt ebdb-save-on-exit t)
            (keymap-set ebdb-mode-map "q" #'kill-this-buffer))
        '';
        elispPackages = with pkgs.emacsPackages; [ ebdb ];
      };
    });
  };
}
