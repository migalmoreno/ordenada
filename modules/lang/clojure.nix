{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkEnableOption mkOption types;
in
{
  options.ordenada.features = {
    clojure = {
      enable = mkEnableOption "the Clojure feature";
    };
    emacs.cider = {
      popReplOnConnect = mkOption {
        type = types.either (types.strMatching "display-only") types.bool;
        description = ''
          If true, pop a REPL buffer and focus on it, if "display-only" pop it but
          don't focus on it, and if false create it but don't display it.
        '';
        default = "display-only";
      };
      replInCurrentWindow = mkEnableOption "showing the REPL in the current window";
    };
  };
  config = {
    home-manager = mkHomeConfig config "clojure" (user: {
      home.packages = with pkgs; [
        clj-kondo
        cljfmt
        zprint
        clojure
        jdk
        leiningen
      ];
      home.file.".zprint.edn".text = ''
        {:search-config? true}
      '';
      programs.emacs = mkElispConfig {
        name = "ordenada-clojure";
        config = with user.features.emacs.cider; ''
          (defgroup ordenada-clojure nil
            "General Clojure programming utilities."
            :group 'ordenada)

          (defun ordenada-clojure--disable-eglot-parts ()
            (setq-local eglot-stay-out-of '(flymake eldoc)))

          (add-hook 'after-init-hook #'jarchive-setup)

          (with-eval-after-load 'cider
            (setopt cider-allow-jack-in-without-project t)
            (setopt cider-use-xref nil)
            (setopt cider-auto-select-error-buffer nil)
            (setopt cider-inspector-auto-select-buffer nil)
            (setopt cider-auto-select-test-report-buffer nil)
            (setq cider-print-options '(("right-margin" 70) ("length" 50)))
            (setopt cider-doc-auto-select-buffer nil))

          (with-eval-after-load 'cider-repl
            (setopt cider-repl-pop-to-buffer-on-connect ${
              if popReplOnConnect == "display-only" then
                "'display-only"
              else if popReplOnConnect then
                "t"
              else
                "nil"
            })
            (setopt cider-repl-display-in-current-window ${mkBoolean replInCurrentWindow})
            (setopt cider-repl-display-help-banner nil))

          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "J" #'clj-deps-new))

          (with-eval-after-load 'org
            (add-to-list 'org-structure-template-alist
                         '("clj" . "src clojure")))

          (with-eval-after-load 'ob-core
            (require 'ob-clojure)
            (require 'ob-java))

          (with-eval-after-load 'ob-clojure
            (setopt org-babel-clojure-backend 'cider))

          (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs
                         '(((clojure-mode :language-id "clojure")
                            (clojurec-mode :language-id "clojure")
                            (clojurescript-mode :language-id "clojurescript"))
                           . ("${pkgs.clojure-lsp}/bin/clojure-lsp"))))

          (define-minor-mode ordenada-clojure-mode
            "Set up convenient tweaks for Clojure development."
            :group 'ordenada-clojure
            (when ordenada-clojure-mode
              (ordenada-clojure--disable-eglot-parts)
              (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
              (eglot-ensure)
              (flymake-kondor-setup)
              (flymake-mode t)))

          (add-hook 'clojure-mode-hook #'ordenada-clojure-mode)

          (with-eval-after-load 'apheleia
            (push '(zprint . ("zprint")) apheleia-formatters)
            (add-to-list 'apheleia-mode-alist '(clojure-mode . zprint)))
        '';
        elispPackages = with pkgs.emacsPackages; [
          cider
          clojure-mode
          jarchive
          html-to-hiccup
          clj-deps-new
          flymake-kondor
        ];
      };
    });
  };
}
