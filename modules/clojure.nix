{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options.ordenada.features.clojure = {
    enable = lib.mkEnableOption "the Clojure feature";
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
      programs.emacs = mkElispConfig {
        name = "ordenada-clojure";
        config = ''
          (defgroup ordenada-clojure nil
            "General Clojure programming utilities."
            :group 'ordenada)

          (defun ordenada-clojure--disable-eglot-parts ()
            (setq-local eglot-stay-out-of '(flymake eldoc)))

          (add-hook 'after-init-hook #'jarchive-setup)

          (with-eval-after-load 'cider
            (setq cider-allow-jack-in-without-project t)
            (setq cider-use-xref nil)
            (setq cider-auto-select-error-buffer nil)
            (setq cider-inspector-auto-select-buffer nil)
            (setq cider-auto-select-test-report-buffer nil)
            (setq cider-print-options '(("right-margin" 70) ("length" 50)))
            (setq cider-doc-auto-select-buffer nil))

          (with-eval-after-load 'cider-repl
            (define-key cider-repl-mode-map (kbd "C-M-q") #'indent-sexp)
            (setq cider-repl-pop-to-buffer-on-connect #'display-only)
            (setq cider-repl-display-help-banner nil))

          (with-eval-after-load 'ordenada-keymaps
            (define-key ordenada-app-map (kbd "J") #'clj-deps-new))

          (with-eval-after-load 'org
            (add-to-list 'org-structure-template-alist
                         '("clj" . "src clojure")))

          (with-eval-after-load 'ob-core
            (require 'ob-clojure)
            (require 'ob-java)
            (setq org-babel-default-header-args:clojure
                  '((:results . "scalar")
                    (:session . ""))))

          (with-eval-after-load 'ob-clojure
            (setq org-babel-clojure-backend 'cider))

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
              (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
              (eglot-ensure)
              (flymake-kondor-setup)
              (flymake-mode t)))

          (add-hook 'clojure-mode-hook #'ordenada-clojure-mode)
          (with-eval-after-load 'clojure-mode
            (setq clojure-align-forms-automatically t))

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
