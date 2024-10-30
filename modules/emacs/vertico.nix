{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.vertico = {
      enable = lib.mkEnableOption "the Emacs vertico feature";
      package = lib.mkOption {
        type = lib.types.package;
        description = "The Vertico package to use.";
        default = pkgs.emacsPackages.vertico;
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.vertico" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-vertico";
        config = ''
          (eval-when-compile
            (require 'vertico)
            (require 'vertico-multiform))

          (with-eval-after-load 'vertico
            (advice-add
             'vertico--format-candidate :around
             (lambda (orig cand prefix suffix index _start)
               (let ((cand (funcall orig cand prefix suffix index _start)))
                 (concat
                  (if (= vertico--index index)
                      (propertize "» " 'face 'vertico-current)
                      "  ")
                  cand))))
            (keymap-global-set "s-s" #'vertico-repeat)
            (require 'vertico-repeat)
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (setopt vertico-cycle t)
            (setopt vertico-multiform-categories
                    '((consult-grep buffer)
                      (imenu buffer)
                      (buffer)
                      ;; (file buffer)
                      ;; (project-file buffer)
                      (info-menu buffer)
                      (consult-org-heading buffer)
                      (consult-history buffer)
                      (consult-lsp-symbols buffer)
                      (consult-xref buffer)
                      (embark-keybinding buffer)
                      (consult-location buffer)))

            (setopt vertico-multiform-commands
                    '((telega-chat-with buffer)
                      (magit:--author flat)
                      ;; For some reason it doesn't have an info-menu
                      ;; category and also setting
                      ;; marginalia-command-categories doesn't help
                      ;; (org-roam-node-find buffer)
                      (Info-goto-node buffer)
                      (info-lookup-symbol buffer)
                      (Info-follow-reference buffer)
                      (consult-yank-pop buffer)))

            (autoload 'vertico-multiform-mode "vertico-multiform")
            (vertico-multiform-mode))

          (autoload 'vertico-mode "vertico")
          (if after-init-time
            (vertico-mode 1)
            (add-hook 'after-init-hook #'vertico-mode))
        '';
        elispPackages = [ user.features.emacs.vertico.package ];
      };
    });
  };
}
