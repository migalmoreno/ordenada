{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.embark = {
      enable = lib.mkEnableOption "the Embark feature.";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.embark" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-embark";
        config = ''
          (define-key global-map (kbd "C-.") #'embark-act)
          (define-key global-map (kbd "C->") #'embark-become)
          (define-key minibuffer-local-map (kbd "M-g") #'embark-become)
          (define-key help-map "b" #'embark-bindings)

          (with-eval-after-load 'embark
            (setq embark-indicators '(embark-minimal-indicator))
            (setq embark-prompter #'embark-keymap-prompter)
            (setq prefix-help-command #'embark-prefix-help-command))

          (with-eval-after-load 'window
            (add-to-list 'display-buffer-alist
                           `(,(rx bos "*Embark Collect "
                                  (or "Live" "Completions") "*")
                             nil
                             (window-parameters (mode-line-format . none)))))
        '';
        elispPackages = with pkgs.emacsPackages; [ embark ];
      };
    });
  };
}
