{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.org = {
      enable = lib.mkEnableOption "the Emacs Org feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.org" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-org";
        config = ''
          (eval-when-compile
            (require 'org)
            (require 'org-refile)
            (require 'org-modern))
          (with-eval-after-load 'org
            (require 'org-tempo)
            (require 'org-timer)
            (let ((map org-mode-map))
              (define-key map (kbd "M-n") 'org-metaright)
              (define-key map (kbd "M-p") 'org-metaleft))
            (setq org-startup-folded 'content)
            (setq org-startup-indented t)
            (setq org-startup-with-inline-images t)
            (setq org-extend-today-until 0)
            (setq org-use-fast-todo-selection 'expert)
            (setq org-log-done 'time)
            (setq org-special-ctrl-a/e t)
            (setq org-insert-heading-respect-content t)
            (setq org-auto-align-tags t)
            (setq org-tags-exclude-from-inheritance '("todo" "crypt"))
            (setq org-enforce-todo-dependencies t)
            (setq org-enforce-todo-checkbox-dependencies t)
            (setq org-archive-location "~/documents/archive.org::* From %s")
            (setq org-fast-tag-selection-single-key 'expert)
            (setq org-display-remote-inline-images 'cache)
            (setq org-image-actual-width nil)
            (setq org-pretty-entities t)
            (setq org-pretty-entities-include-sub-superscripts nil)
            (setq org-M-RET-may-split-line nil)
            (setq org-highest-priority ?A)
            (setq org-lowest-priority ?C)
            (setq org-default-priority ?B)
            (setq org-fontify-done-headline t)
            (setq org-adapt-indentation nil)
            (setq org-edit-src-content-indentation 0)
            (setq org-outline-path-complete-in-steps nil)
            (setq org-refile-use-outline-path 'full-file-path)
            (setq org-refile-targets `((nil . (:maxlevel . 3))
                                       (org-agenda-files . (:maxlevel . 3))))
            (setq org-ellipsis "⤵")
            (setq org-hide-emphasis-markers t)
            (setq org-log-into-drawer t)
            (setq org-directory "~/notes")
            (setq org-default-notes-file (concat org-directory "/todo.org"))
            (setq org-M-RET-may-split-line nil))

          (with-eval-after-load 'window
            (add-to-list 'display-buffer-alist
                         `(,(rx "*Org Links*")
                           display-buffer-no-window
                           (allow-no-window . t))))

          (with-eval-after-load 'org-capture
            (setq org-capture-bookmark nil))

          (with-eval-after-load 'org-download
            (setq org-download-image-dir "images")
            (setq org-download-image-org-width 300))

          (with-eval-after-load 'org-id
            (setq org-id-locations-file
                  (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                           "/emacs/org-id-locations")))

          (with-eval-after-load 'org-keys
            (setq org-return-follows-link t))

          (with-eval-after-load 'org-list
            (setq org-list-demote-modify-bullet
                  '(("+" . "-") ("-" . "+") ("*" . "+"))))

          (with-eval-after-load 'org-src
            (setq org-src-tab-acts-natively t)
            (setq org-src-window-setup 'current-window)
            (setq org-catch-invisible-edits 'show-and-error)
            (setq org-src-fontify-natively t))

          (with-eval-after-load 'ob-core
            (setq org-confirm-babel-evaluate nil))

          (with-eval-after-load 'ol
            (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))

          (with-eval-after-load 'ox
            (setq org-export-preserve-breaks t))

          (add-hook 'org-mode-hook 'org-appear-mode)
          (autoload 'global-org-modern-mode "org-modern")
          (if after-init-time
              (global-org-modern-mode)
              (add-hook 'after-init-hook 'global-org-modern-mode))
          (with-eval-after-load 'org-modern
            (setq org-modern-todo nil)
            (setq org-modern-timestamp nil)
            (setq org-modern-statistics nil)
            (setq org-modern-tag nil)
            (setq org-modern-priority nil)
            (setq org-modern-hide-stars nil)
            (setq org-hide-leading-stars t))

          (add-hook 'org-mode-hook 'olivetti-mode)
          (add-hook 'org-mode-hook 'prettify-symbols-mode)
          (add-hook 'org-mode-hook 'variable-pitch-mode)
        '';
        elispPackages = with pkgs.emacsPackages; [
          olivetti
          org
          org-appear
          org-contrib
          org-modern
        ];

      };
    });
  };
}
