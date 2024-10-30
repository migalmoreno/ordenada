{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.help = {
      enable = lib.mkEnableOption "the Emacs help feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.help" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-help";
        config = ''
          (with-eval-after-load 'window
            (setopt split-window-keep-point t)
            (add-to-list 'display-buffer-alist
                         `(,(rx "*help" (* any) "*")
                           (display-buffer-reuse-window
                            display-buffer-same-window)
                           (reusable-frames . t))))

          (add-hook 'help-mode-hook #'visual-line-mode)

          (let ((map goto-map))
            (keymap-set map "L" #'find-library)
            (keymap-set map "F" #'find-function)
            (keymap-set map "K" #'find-function-on-key)
            (keymap-set map "V" #'find-variable))

          (let ((map global-map))
            (define-key map [remap describe-function] #'helpful-callable)
            (define-key map [remap describe-variable] #'helpful-variable)
            (define-key map [remap describe-key] #'helpful-key)
            (define-key map [remap describe-command] #'helpful-command)
            (define-key map [remap Info-goto-emacs-command-node] #'helpful-function))
          (keymap-set help-map "o" #'helpful-at-point)

          (with-eval-after-load 'embark
            (define-key embark-symbol-map [remap describe-symbol] #'helpful-symbol)
            (let ((map embark-become-help-map))
              (define-key map [remap describe-function] #'helpful-callable)
              (define-key map [remap describe-variable] #'helpful-variable)
              (define-key map [remap describe-symbol] #'helpful-symbol)
              (define-key map [remap describe-command] #'helpful-command)))
          (add-hook 'helpful-mode-hook #'visual-line-mode)
          (with-eval-after-load 'helpful
            (keymap-set helpful-mode-map "q" #'kill-this-buffer))
          (with-eval-after-load 'help-mode
            (keymap-set help-mode-map "q" #'kill-this-buffer)
            (setopt help-window-select t))
        '';
        elispPackages = with pkgs.emacsPackages; [ helpful ];
      };
    });
  };
}
