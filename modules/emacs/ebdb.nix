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
      sources = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "The list of EBDB database sources.";
        default = [ "${config.ordenada.features.xdg.userDirs.documents}/contacts" ];
      };
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
            (setq ebdb-sources ${mkList user.features.emacs.ebdb.sources})
            (setq ebdb-default-country nil)
            (setq ebdb-default-window-size 0.4)
            (setq ebdb-dedicated-window 'ebdb)
            (setq ebdb-mail-avoid-redundancy t)
            (setq ebdb-complete-mail 'capf)
            (setq ebdb-completion-display-record nil)
            (setq ebdb-complete-mail-allow-cycling nil)
            (setq ebdb-save-on-exit t)
            (keymap-set ebdb-mode-map "q" #'kill-this-buffer))
        '';
        elispPackages = with pkgs.emacsPackages; [ ebdb ];
      };
    });
  };
}
