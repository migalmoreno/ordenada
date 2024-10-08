{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.eshell = {
      enable = lib.mkEnableOption "the Emacs eshell feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.eshell" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-eshell";
        config = ''
          (eval-when-compile
            (require 'eshell)
            (require 'em-alias)
            (require 'em-hist)
            (require 'eshell-prompt-extras))

          (defgroup ordenada-eshell nil
            "Eshell customizations for a better integration with Emacs tooling."
            :group 'ordenada)

          (define-minor-mode ordenada-eshell-mode
            "Set up environment on `eshell-mode' invocation."
            :group 'ordenada-eshell
            (if ordenada-eshell-mode
                (progn
                  (if (and (boundp 'envrc-global-mode) envrc-global-mode)
                      (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
                      (setenv "PAGER" ""))
                  (define-key eshell-mode-map (kbd "C-c M-o") 'eshell/clear))
                (local-unset-key 'eshell/clear)))

          (defun ordenada--epe-git-branch ()
            "Return your git branch name."
            (let ((branch (car (vc-git-branches))))
              (cond
               ((null branch) "no-branch")
               ((string-match "^(HEAD detached at \\(.+\\))$" branch)
                (concat epe-git-detached-HEAD-char
                       (truncate-string-to-width
                         (match-string 1 branch) 25 nil nil "...")))
               (t (truncate-string-to-width branch 25 nil nil "...")))))

          (advice-add 'epe-git-branch :override #'ordenada--epe-git-branch)

          (define-key global-map (kbd "s-e") 'eshell)
          (add-hook 'eshell-mode-hook 'ordenada-eshell-mode)
          (with-eval-after-load 'eshell
            (let ((eshell-cache (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                        "/emacs/eshell/")))
              (setq eshell-aliases-file (concat eshell-cache "alias"))
              (setq eshell-history-file-name (concat eshell-cache "history"))
              (setq eshell-last-dir-ring-file-name
                    (concat eshell-cache "lastdir")))
            (setq eshell-banner-message "")
            (autoload 'eshell-syntax-highlighting-global-mode
                      "eshell-syntax-highlighting")
            (eshell-syntax-highlighting-global-mode)
            (add-hook 'eshell-hist-mode-hook
                      (lambda ()
                        (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history)))

            (add-hook 'eshell-preoutput-filter-functions #'ansi-color-filter-apply)
            (with-eval-after-load 'em-prompt
              (autoload 'epe-theme-lambda "eshell-prompt-extras")
              (setq eshell-prompt-function #'epe-theme-lambda)
              (setq eshell-highlight-prompt nil)))
        '';
        elispPackages = with pkgs.emacsPackages; [
          eshell-syntax-highlighting
          eshell-prompt-extras
        ];
      };
    });
  };
}
