{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkEnableOption mkOption types;
in
{
  options = {
    ordenada.features.emacs.org = {
      enable = mkEnableOption "the Emacs Org feature";
      directory = mkOption {
        type = types.str;
        description = "The directory where Org should look for files.";
        default = "${config.ordenada.features.xdg.userDirs.documents}/org";
      };
      startupIndented = mkEnableTrueOption "turning org-indent-mode on startup";
      orgModern = mkEnableOption "Org Modern integration styles in Org buffers";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.org" (
      user: with user.features.emacs.org; {
        programs.emacs = mkElispConfig {
          name = "ordenada-org";
          config = ''
            (eval-when-compile
              (require 'org)
              (require 'org-refile)
              (require 'org-modern)
              (require 'cl-macs))

            (cl-defun ordenada-org-do-promote (&optional (levels 1))
              "Allow promoting the current heading LEVELS high up the tree."
              (interactive "p")
              (save-excursion
                (if (org-region-active-p)
                    (org-map-region (lambda ()
                                      (dotimes (_ levels)
                                        (org-promote)))
                                    (region-beginning) (region-end))
                  (dotimes (_ levels)
                    (org-promote))))
              (org-fix-position-after-promote))
            (advice-add 'org-do-promote :override #'ordenada-org-do-promote)

            (with-eval-after-load 'org
              (require 'org-tempo)
              (require 'org-timer)

              (keymap-set org-mode-map "M-n" #'org-metaright)
              (keymap-set org-mode-map "M-p" #'org-metaleft)
              (setopt org-startup-folded 'content)
              (setopt org-startup-indented ${mkBoolean startupIndented})
              (setopt org-startup-with-inline-images t)
              (setopt org-extend-today-until 0)
              (setopt org-use-fast-todo-selection 'expert)
              (setopt org-log-done 'time)
              (setopt org-special-ctrl-a/e t)
              (setopt org-insert-heading-respect-content t)
              (setopt org-auto-align-tags t)
              (setopt org-enforce-todo-dependencies t)
              (setopt org-enforce-todo-checkbox-dependencies t)
              (setopt org-fast-tag-selection-single-key 'expert)
              (setopt org-display-remote-inline-images 'cache)
              (setopt org-image-actual-width nil)
              (setopt org-pretty-entities t)
              (setopt org-pretty-entities-include-sub-superscripts nil)
              (setopt org-M-RET-may-split-line nil)
              (setopt org-highest-priority ?A)
              (setopt org-lowest-priority ?C)
              (setopt org-default-priority ?B)
              (setopt org-fontify-done-headline t)
              (setopt org-adapt-indentation nil)
              (setopt org-ellipsis "⤵")
              (setopt org-hide-emphasis-markers t)
              (setopt org-log-into-drawer t)
              (setopt org-directory "${directory}")
              (setopt org-default-notes-file "${directory}/todo.org")
              (setopt org-archive-location "${directory}/archive.org::* From %s")
              (setopt org-M-RET-may-split-line nil))

            (with-eval-after-load 'window
              (add-to-list 'display-buffer-alist
                           `(,(rx "*Org Links*")
                             display-buffer-no-window
                             (allow-no-window . t))))

            (with-eval-after-load 'org-capture
              (setopt org-capture-bookmark nil))

            (with-eval-after-load 'org-download
              (setopt org-download-image-dir "images")
              (setopt org-download-image-org-width 300))

            (with-eval-after-load 'org-refile
              (setopt org-outline-path-complete-in-steps nil)
              (setopt org-refile-use-outline-path 'full-file-path)
              (setopt org-refile-targets `((nil . (:maxlevel . 3))
                                           (org-agenda-files . (:maxlevel . 3)))))

            (with-eval-after-load 'org-id
              (setopt org-id-locations-file
                      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                               "/emacs/org-id-locations")))

            (with-eval-after-load 'org-keys
              (setopt org-return-follows-link t))

            (with-eval-after-load 'org-list
              (setopt org-list-demote-modify-bullet
                      '(("+" . "-") ("-" . "+") ("*" . "+"))))

            (with-eval-after-load 'org-src
              (setopt org-edit-src-content-indentation 0)
              (setopt org-src-tab-acts-natively t)
              (setopt org-src-window-setup 'current-window)
              (setopt org-catch-invisible-edits 'show-and-error)
              (setopt org-src-fontify-natively t))

            (org-crypt-use-before-save-magic)
            (with-eval-after-load 'org-crypt
              (add-to-list 'org-tags-exclude-from-inheritance "crypt"))

            (with-eval-after-load 'ob-core
              (setopt org-confirm-babel-evaluate nil))

            (keymap-set mode-specific-map "l" #'org-store-link)
            (with-eval-after-load 'ol
              (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))

            (with-eval-after-load 'ox
              (setopt org-export-preserve-breaks t))

            (add-hook 'org-mode-hook #'org-appear-mode)
            ${mkIf orgModern ''
              (autoload 'global-org-modern-mode "org-modern")
              (if after-init-time
                  (global-org-modern-mode)
                  (add-hook 'after-init-hook #'global-org-modern-mode))
              (with-eval-after-load 'org-modern
                (setopt org-modern-todo nil)
                (setopt org-modern-timestamp nil)
                (setopt org-modern-statistics nil)
                (setopt org-modern-tag nil)
                (setopt org-modern-priority nil)
                (setopt org-modern-hide-stars nil)
                (setopt org-hide-leading-stars t))
            ''}
            (add-hook 'org-mode-hook #'olivetti-mode)
            (add-hook 'org-mode-hook #'prettify-symbols-mode)
            (add-hook 'org-mode-hook #'variable-pitch-mode)
          '';
          elispPackages =
            with pkgs.emacsPackages;
            [
              olivetti
              org
              org-appear
              org-contrib
            ]
            ++ lib.optional orgModern org-modern;
        };
      }
    );
  };
}
