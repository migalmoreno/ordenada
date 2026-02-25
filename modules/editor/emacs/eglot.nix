{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  inherit (lib)
    mkIf
    mkOption
    types
    ;
in
mkFeature {
  name = [
    "emacs"
    "eglot"
  ];
  options = {
    connectTimeout = mkOption {
      type = types.int;
      default = 60;
      description = "Number of seconds before timing out LSP connection attempts.";
    };
    inlineSuggestions = mkOption {
      type = types.bool;
      default = true;
      description = "Whether eglot should show code action suggestions for the current line.";
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-eglot";
        config = with config.ordenada.features.emacs.eglot;  # elisp
          ''
            (with-eval-after-load 'eglot
              (unless (display-graphic-p)
                (setq eglot-code-action-indicator "\u2139")) ;; "i" icon

              ${if (inlineSuggestions != true) then ''
                (setq eglot-code-action-indications '())
              '' else ""}

              (setopt eglot-confirm-server-edits nil)
              (setopt eglot-extend-to-xref t)
              (setopt eglot-connect-timeout ${toString config.ordenada.features.emacs.eglot.connectTimeout})
              (let ((map eglot-mode-map))
                (keymap-set map "C-c c a" '("Actions" . eglot-code-actions))
                (keymap-set map "C-c c o" '("Organize imports" . eglot-code-action-organize-imports))
                (keymap-set map "C-c c q" '("Quickfix" . eglot-code-action-quickfix))
                (keymap-set map "C-c c r" '("Rename symbol" . eglot-rename))
                (keymap-set map "C-c c f" '("Format buffer" . eglot-format))))
          '';
        elispPackages =
          with pkgs.emacsPackages;
          [ eglot ] ++ lib.optional config.ordenada.features.emacs.consult.enable consult-eglot;
      };
    };
}
