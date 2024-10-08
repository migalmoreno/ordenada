{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  userSwitches =
    if config.ordenada.features.emacs.advancedUser then [ "-A --time-style=long-iso" ] else [ "-a" ];
  listingSwitches =
    with config.ordenada.features.emacs.dired;
    [ "-l" ]
    ++ (if groupDirsFirst then [ "--group-directories-first" ] else [ ])
    ++ (if extraSwitches != [ ] then userSwitches else [ ]);
in
{
  options = {
    ordenada.features.emacs.dired = {
      enable = lib.mkEnableOption "the Emacs Dired feature";
      groupDirsFirst = mkEnableTrueOption "sorting directories first in Dired listing";
      killOnNewBuffer = lib.mkEnableOption "killing current Dired buffer on opening new buffer";
      extraSwitches = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "The list of extra switches passed to ls for Dired.";
        default = [ "-h" ];
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.dired" (user: {
      home.packages = with pkgs; [
        zip
        unzip
        rsync
      ];
      programs.emacs = mkElispConfig {
        name = "ordenada-dired";
        config = with user.features.emacs; ''
          (eval-when-compile (require 'dired))
          ${
            if (hasFeature "emacs.embark" user) then
              ''
                (defun ordenada-dired-open-externally ()
                  "Open marked files in Dired through an external program."
                  (interactive)
                  (let ((files (dired-get-marked-files)))
                    (mapc 'embark-open-externally files)))
                (with-eval-after-load 'dired
                  (define-key dired-mode-map "V" #'ordenada-dired-open-externally))
              ''
            else
              ""
          }
          (define-key global-map (kbd "s-d") 'dired-jump)
          ${
            if advancedUser then
              ''
                (add-hook 'dired-mode-hook 'dired-hide-details-mode)
              ''
            else
              ""
          }
          (add-hook 'dired-mode-hook 'toggle-truncate-lines)
          (with-eval-after-load 'dired
            ${
              if (hasFeature "emacs.all-the-icons" user) then
                ''
                  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
                  (with-eval-after-load 'all-the-icons-dired
                    (setq all-the-icons-dired-monochrome nil))
                ''
              else
                ""
            } 
            (let ((map dired-mode-map))
              (define-key map (kbd "C-c C-r") #'dired-rsync)
              (define-key map "q" #'kill-current-buffer))
            (setq dired-dwim-target t)
            (setq dired-listing-switches "${toString listingSwitches}")
            ${
              if dired.killOnNewBuffer then
                ''
                  (setq dired-kill-when-opening-new-dired-buffer t)
                ''
              else
                ""
            } 
            (setq dired-hide-details-hide-symlink-targets nil)
            (setq delete-by-moving-to-trash nil)
            (setq dired-recursive-deletes 'always)
            (setq dired-clean-confirm-killing-deleted-buffers nil)
            (setq dired-recursive-copies 'always))

          (with-eval-after-load 'dired-rsync
            (setq dired-rsync-options
                  "--exclude .git/ --exclude .gitignore -az --info=progress2 --delete"))
          (with-eval-after-load 'ls-lisp
            (setq ls-lisp-use-insert-directory-program nil))
        '';
        elispPackages = with pkgs.emacsPackages; [
          all-the-icons-dired
          dired-rsync
        ];
      };
    });
  };
}
