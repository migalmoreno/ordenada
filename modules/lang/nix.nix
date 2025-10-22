{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "nix";
  options = {
    polymode = lib.mkEnableOption "Polymode support for Nix multi-line strings";
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
        nil
        nixfmt-rfc-style
      ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-nix";
        config = # elisp
          ''
            (add-hook 'nix-mode-hook #'eglot-ensure)
            (with-eval-after-load 'eglot
              (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))
            ${lib.optionalString config.ordenada.features.nix.polymode ''
              (eval-and-compile
                (require 'polymode))
              (define-hostmode ordenada-nix-hostmode
                :mode 'nix-mode)
              (define-auto-innermode ordenada-nix-dynamic-innermode
                :head-matcher (rx "#" blank (+ (any "a-z" "-")) (+ (any "\n" blank)) "'''")
                :tail-matcher (rx bol (+ blank) "''';")
                :mode-matcher (cons (rx "#" blank (group (+ (any "a-z" "-"))) (+ (any "\n" blank)) "'''") 1)
                :head-mode 'host
                :tail-mode 'host)
              (define-polymode ordenada-nix-polymode
                :hostmode 'ordenada-nix-hostmode
                :innermodes '(ordenada-nix-dynamic-innermode))

              (add-to-list 'auto-mode-alist '("\\.nix" . ordenada-nix-polymode))
            ''}
            (with-eval-after-load 'smartparens
              (add-hook 'nix-mode-hook #'turn-off-smartparens-mode))
            (add-hook 'nix-mode-hook #'electric-pair-local-mode)
          '';
        elispPackages =
          with pkgs.emacsPackages;
          [ nix-mode ] ++ (lib.optional config.ordenada.features.nix.polymode [ polymode ]);
      };
    };
}
