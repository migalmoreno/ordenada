{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "programming"
  ];
  options = with lib; {
    rainbow-delimiters.enable = mkEnableOption "the Emacs rainbow-delimiters feature";
    apheleia.enable = mkEnableOption "the Emacs Apheleia feature";
    eglot.enable = mkEnableOption "the Emacs Eglot feature";
    flymake.enable = mkEnableOption "the Emacs Flymake feature";
  };
  homeManager =
    { config, pkgs, ... }:
    let
      inherit (ordenada-lib) mkElispConfig;
    in
    {
      programs.emacs = lib.mkMerge (
        with config.ordenada.features.emacs.programming;
        [
          (lib.mkIf rainbow-delimiters.enable (
            mkElispConfig pkgs {
              name = "ordenada-rainbow-delimiters";
              config = ''
                (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
              '';
              elispPackages = with pkgs.emacsPackages; [ rainbow-delimiters ];
            }
          ))
          (lib.mkIf apheleia.enable (
            mkElispConfig pkgs {
              name = "ordenada-apheleia";
              config = ''
                (add-hook 'after-init-hook #'apheleia-global-mode)
              '';
              elispPackages = with pkgs.emacsPackages; [ apheleia ];
            }
          ))
          (lib.mkIf eglot.enable (
            mkElispConfig pkgs {
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
                [ eglot ] ++ lib.optional config.ordenada.features.emacs.consult.enable consult-eglot;
            }
          ))
          (lib.mkIf flymake.enable (
            mkElispConfig pkgs {
              name = "ordenada-flymake";
              config = ''
                (with-eval-after-load 'flymake
                  (let ((map flymake-mode-map))
                    (keymap-set map "M-n" #'flymake-goto-next-error)
                    (keymap-set map "M-p" #'flymake-goto-prev-error)))
              '';
            }
          ))
        ]
      );
    };
}
