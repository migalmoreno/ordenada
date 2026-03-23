{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  inherit (lib)
    mkIf
    ;
in
mkFeature {
  name = "nix";
  options = {
    enablePolymode = ordenada-lib.mkEnableTrueOption "Polymode support for Nix multi-line strings";
  };
  nixos = {
    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes
        warn-dirty = false
      '';
    };
    nixpkgs.config.allowUnfree = true;
  };
  homeManager =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [
        nixfmt-rfc-style
      ];

      ordenada.features.emacs.corfu.globalModes = mkIf (config.ordenada.features.emacs.corfu.enable) [
        "nix-mode"
        "nix-ts-mode"
      ];

      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-nix";
        config = # elisp
          ''
            (defgroup ordenada-nix nil
              "General nix programming utilities."
              :group 'ordenada)

            (defvar ordenada-nix-mode-map (make-sparse-keymap))

            (keymap-set ordenada-nix-mode-map "C-c f"
                        '("Format buffer" . nix-format-buffer))

            (define-minor-mode ordenada-nix-mode
              "Set up convenient tweaks for nix development."
              :group 'ordenada-nix :keymap ordenada-nix-mode-map
              (when ordenada-javascript-mode
                (eglot-ensure)
                (electric-pair-local-mode)))

            (add-hook 'nix-ts-mode-hook #'ordenada-nix-mode)

            (with-eval-after-load 'eglot
              (add-to-list 'eglot-server-programs '(nix-ts-mode . ("${lib.getExe pkgs.nil}"))))

            (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))

            (eval-and-compile
              (require 'nix-mode)
              (require 'nix-format))

            (with-eval-after-load 'nix-format
              (setopt nix-nixfmt-bin "${pkgs.nixfmt}/bin/nixfmt"))

            ${lib.optionalString config.ordenada.features.nix.enablePolymode ''
              (eval-and-compile
                (require 'polymode))
              (define-hostmode ordenada-nix-hostmode
                :mode 'nix-ts-mode)
              (define-auto-innermode ordenada-nix-dynamic-innermode
                :head-matcher (cons "^.*\\(#[[:blank:]]+[a-z-]+\n[[:blank:]]*'''\\)" 1)
                :tail-matcher "^[ \t]+'''[ \n]*)?;"
                :mode-matcher (cons "#[ \t]*\\([a-z-]+\\)?\\([^ \n']*\\)" 1)
                :body-indent-offset 4
                :head-mode 'host
                :tail-mode 'host)
              (define-polymode ordenada-nix-polymode
                :hostmode 'ordenada-nix-hostmode
                :innermodes '(ordenada-nix-dynamic-innermode))

              (add-to-list 'auto-mode-alist '("\\.nix" . ordenada-nix-polymode))

              (with-eval-after-load 'rainbow-delimiters
                (add-hook 'nix-ts-mode-hook #'rainbow-delimiters-mode-disable))
            ''}

            (with-eval-after-load 'smartparens
              (add-hook 'nix-ts-mode-hook #'turn-off-smartparens-mode))
          '';
        elispPackages =
          with pkgs.emacsPackages;
          [
            nix-mode
            nix-ts-mode
            (treesit-grammars.with-grammars (
              grammars: with grammars; [
                tree-sitter-nix
              ]
            ))
          ]
          ++ (lib.optional config.ordenada.features.nix.enablePolymode polymode);
      };
    };
}
