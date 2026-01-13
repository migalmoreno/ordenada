{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "eglot"
  ];
  options = {
    connectTimeout = lib.mkOption {
      type = lib.types.int;
      default = 60;
      description = "Number of seconds before timing out LSP connection attempts.";
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-eglot";
        config = # elisp
          ''
            (with-eval-after-load 'eglot
              (setopt eglot-confirm-server-edits nil)
              (setopt eglot-extend-to-xref t)
              (setopt eglot-connect-timeout ${toString config.ordenada.features.emacs.eglot.connectTimeout})
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
      };
    };
}
