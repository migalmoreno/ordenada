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
      groupDirsFirst = mkEnableTrueOption "sorting directories first in the Dired listing";
      killOnNewBuffer = lib.mkEnableOption "killing the current Dired buffer on opening a new buffer";
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
          ${mkIf (hasFeature "emacs.embark" user) ''
            (defun ordenada-dired-open-externally ()
              "Open marked files in Dired through an external program."
              (interactive)
              (let ((files (dired-get-marked-files)))
                (mapc #'embark-open-externally files)))

            (autoload 'embark-open-externally "embark")
            (with-eval-after-load 'dired
              (keymap-set dired-mode-map "V" #'ordenada-dired-open-externally))
          ''}
          (keymap-global-set "s-d" #'dired-jump)
          ${mkIf advancedUser ''
            (add-hook 'dired-mode-hook 'dired-hide-details-mode)
          ''}
          (add-hook 'dired-mode-hook 'toggle-truncate-lines)
          (with-eval-after-load 'dired
            ${mkIf (hasFeature "emacs.all-the-icons" user) ''
              (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
              (with-eval-after-load 'all-the-icons-dired
                (setopt all-the-icons-dired-monochrome nil))
            ''}
            (let ((map dired-mode-map))
              (keymap-set map "C-c C-r" #'dired-rsync)
              (keymap-set map "q" #'kill-current-buffer))
            (setopt dired-dwim-target t)
            (setopt dired-listing-switches "${toString listingSwitches}")
            ${mkIf dired.killOnNewBuffer ''
              (setopt dired-kill-when-opening-new-dired-buffer t)
            ''}
            (setopt dired-hide-details-hide-symlink-targets nil)
            (setopt delete-by-moving-to-trash nil)
            (setopt dired-recursive-deletes 'always)
            (setopt dired-clean-confirm-killing-deleted-buffers nil)
            (setopt dired-recursive-copies 'always))

          (with-eval-after-load 'dired-rsync
            (setopt dired-rsync-options
                    "--exclude .git/ --exclude .gitignore -az --info=progress2 --delete"))
          (with-eval-after-load 'ls-lisp
            (setopt ls-lisp-use-insert-directory-program nil))
        '';
        elispPackages = with pkgs.emacsPackages; [
          all-the-icons-dired
          dired-rsync
        ];
      };
    });
  };
}
