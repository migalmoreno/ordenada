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
            (setq split-window-keep-point t)
            (add-to-list 'display-buffer-alist
                         `(,(rx "*help" (* any) "*")
                           (display-buffer-reuse-window
                            display-buffer-same-window)
                           (reusable-frames . t))))

          (add-hook 'help-mode-hook #'visual-line-mode)

          (let ((map goto-map))
            (define-key map "L" #'find-library)
            (define-key map "F" #'find-function)
            (define-key map "K" #'find-function-on-key)
            (define-key map "V" #'find-variable))

          (let ((map global-map))
            (define-key map [remap describe-function] #'helpful-callable)
            (define-key map [remap describe-variable] #'helpful-variable)
            (define-key map [remap describe-key] #'helpful-key)
            (define-key map [remap describe-command] #'helpful-command)
            (define-key map [remap Info-goto-emacs-command-node] #'helpful-function))
          (define-key help-map "o" #'helpful-at-point)

          (with-eval-after-load 'embark
            (define-key embark-symbol-map [remap describe-symbol] #'helpful-symbol)
            (let ((map embark-become-help-map))
              (define-key map [remap describe-function] #'helpful-callable)
              (define-key map [remap describe-variable] #'helpful-variable)
              (define-key map [remap describe-symbol] #'helpful-symbol)
              (define-key map [remap describe-command] #'helpful-command)))
          (add-hook 'helpful-mode-hook #'visual-line-mode)
          (with-eval-after-load 'helpful
            (define-key helpful-mode-map "q" #'kill-this-buffer))
          (with-eval-after-load 'help-mode
            (define-key help-mode-map "q" #'kill-this-buffer)
            (setq help-window-select t))
        '';
        elispPackages = with pkgs.emacsPackages; [ helpful ];
      };
    });
  };
}
