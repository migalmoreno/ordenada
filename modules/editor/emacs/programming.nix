{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs = {
      rainbow-delimiters = {
        enable = lib.mkEnableOption "the Emacs rainbow-delimiters feature";
      };
      apheleia = {
        enable = lib.mkEnableOption "the Emacs Apheleia feature";
      };
      eglot = {
        enable = lib.mkEnableOption "the Emacs Eglot feature";
      };
      flymake = {
        enable = lib.mkEnableOption "the Emacs Flymake feature";
      };
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
              (setopt eglot-confirm-server-edits nil)
              (setopt eglot-extend-to-xref t)
              (let ((map eglot-mode-map))
                (keymap-set map "C-c c a" #'eglot-code-actions)
                (keymap-set map "C-c c o" #'eglot-code-action-organize-imports)
                (keymap-set map "C-c c q" #'eglot-code-action-quickfix)
                (keymap-set map "C-c c r" #'eglot-rename)
                (keymap-set map "C-c c f" #'eglot-format)))
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
                (keymap-set map "M-n" #'flymake-goto-next-error)
                (keymap-set map "M-p" #'flymake-goto-prev-error)))
          '';
        };
      });
    }
  ];
}
