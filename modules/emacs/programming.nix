{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.rainbow-delimiters = {
      enable = lib.mkEnableOption "the Emacs rainbow-delimiters feature.";
    };
    ordenada.features.emacs.apheleia = {
      enable = lib.mkEnableOption "the Emacs Apheleia feature.";
    };
    ordenada.features.emacs.eglot = {
      enable = lib.mkEnableOption "the Emacs Eglot feature.";
    };
    ordenada.features.emacs.flymake = {
      enable = lib.mkEnableOption "the Emacs Flymake feature.";
    };
  };
  config = lib.mkMerge [
    {
      home-manager = mkHomeConfig config "emacs.rainbow-delimiters" (user: {
        programs.emacs = mkElispConfig {
          name = "ordenada-rainbow-delimiters";
          config = ''
            (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
          '';
          elispPackages = with pkgs.emacsPackages; [ rainbow-delimiters ];
        };
      });
    }
    {
      home-manager = mkHomeConfig config "emacs.apheleia" (user: {
        programs.emacs = mkElispConfig {
          name = "ordenada-apheleia";
          config = ''
            (add-hook 'after-init-hook #'apheleia-global-mode)
          '';
          elispPackages = with pkgs.emacsPackages; [ apheleia ];
        };
      });
    }
    {
      home-manager = mkHomeConfig config "emacs.eglot" (user: {
        programs.emacs = mkElispConfig {
          name = "ordenada-eglot";
          config = ''
            (with-eval-after-load 'eglot
              (setq eglot-confirm-server-edits nil)
              (setq eglot-extend-to-xref t)
              (let ((map eglot-mode-map))
                (define-key map (kbd "C-c c a") #'eglot-code-actions)
                (define-key map (kbd "C-c c o") #'eglot-code-action-organize-imports)
                (define-key map (kbd "C-c c q") #'eglot-code-action-quickfix)
                (define-key map (kbd "C-c c r") #'eglot-rename)
                (define-key map (kbd "C-c c f") #'eglot-format)))
          '';
          elispPackages =
            with pkgs.emacsPackages;
            [ eglot ] ++ lib.optional (hasFeature "emacs.consult" user) consult-eglot;
        };
      });
    }
    {
      home-manager = mkHomeConfig config "emacs.flymake" (user: {
        programs.emacs = mkElispConfig {
          name = "ordenada-flymake";
          config = ''
            (with-eval-after-load 'flymake
              (let ((map flymake-mode-map))
                (define-key map (kbd "M-n") #'flymake-goto-next-error)
                (define-key map (kbd "M-p") #'flymake-goto-prev-error)))
          '';
        };
      });
    }
  ];
}
