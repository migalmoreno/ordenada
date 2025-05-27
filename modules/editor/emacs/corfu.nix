{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.corfu = {
      enable = lib.mkEnableOption "the Emacs Corfu feature";
      globalModes = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of modes where Corfu should be enabled.";
        default = [ ];
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.corfu" (user: {
      programs.emacs = pkgs.lib.ordenada.mkElispConfig {
        name = "ordenada-corfu";
        config = with user.features.emacs.corfu; ''
          (autoload 'corfu-history-mode "corfu-history")
          (corfu-history-mode)
          (with-eval-after-load 'corfu
            (let ((map corfu-map))
              (define-key map "\t" #'corfu-next)
              (keymap-set map "<tab>" #'corfu-next)
              (keymap-set map "<backtab>" #'corfu-previous)
              (keymap-set map "S-TAB" #'corfu-previous)
              (keymap-set map "M-p" #'corfu-doc-scroll-down)
              (keymap-set map "M-n" #'corfu-doc-scroll-up)
              (keymap-set map "M-d" #'corfu-doc-toggle))

            (defun ordenada-corfu-move-to-minibuffer ()
              (interactive)
              (let ((completion-extra-properties corfu--extra)
                    completion-cycle-threshold completion-cycling)
                (apply 'consult-completion-in-region completion-in-region--data)))
            (keymap-set corfu-map "M-m" #'ordenada-corfu-move-to-minibuffer)

            (defun ordenada-corfu-enable-in-minibuffer ()
              "Enable Corfu in the minibuffer if `completion-at-point' is bound."
              (when (where-is-internal 'completion-at-point
                                       (list (current-local-map)))
                (corfu-mode 1)))
            (add-hook 'minibuffer-setup-hook #'ordenada-corfu-enable-in-minibuffer)

            (setopt corfu-auto-prefix 2)
            (setopt corfu-min-width 60)
            (setopt corfu-cycle t)
            (setopt corfu-auto t)
            (setopt global-corfu-modes '(${toString globalModes}))
            (global-corfu-mode 1)
            (add-hook 'after-init-hook #'corfu-candidate-overlay-mode)
            (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
            (setopt kind-icon-default-face #'corfu-default)
            (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
            (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch))
        '';
        elispPackages = with pkgs.emacsPackages; [
          cape
          corfu
          corfu-candidate-overlay
        ];
      };
    });
  };
}
