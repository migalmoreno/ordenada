{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

## TODO: - apheleia integration
##       - json5 validation/format (v-jsonlint?)

mkFeature {
  name = "json";
  homeManager =
    { config, pkgs, ... }:
    let
      json5-ts-mode = pkgs.emacsPackages.trivialBuild {
        pname = "json5-ts-mode";
        version = "0.0.0";

        src = builtins.fetchGit {
          url = "https://github.com/dochang/json5-ts-mode.git";
          rev = "8ef36adff943bed504148e54cfff505b92674c10";
        };

        elispFiles = [ "json5-ts-mode.el" ];

        meta = {
          description = "A Emacs tree-sitter major mode for editing JSON5 files";
          homepage = "https://github.com/dochang/json5-ts-mode";
          license = pkgs.lib.licenses.gpl3Plus;
        };
      };
    in
    {
      home.packages = with pkgs; [
        jq
      ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-json";
        config = ''
          (require 'json5-ts-mode)

          (defgroup ordenada-json nil
            "General JSON/JSON5 programming utilities."
            :group 'ordenada)

          (defvar ordenada-json-mode-map (make-sparse-keymap))

          (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
          (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.json5\\'" . json5-ts-mode))

          (define-minor-mode ordenada-json-mode
            "Set up convenient tweaks for JSON/JSON5 development."
            :group 'ordenada-json :keymap ordenada-json-mode-map
            (when ordenada-json-mode
              (eglot-ensure)
              (setq json5-ts-mode-indent-offset 2)))

          (mapcar (lambda (hook)
                    (add-hook (intern (concat (symbol-name hook) "-hook")) 'ordenada-json-mode))
                  '(json-mode json-ts-mode js-json-mode json5-ts-mode))

          (keymap-set ordenada-json-mode-map "C-c f"
                      '("Format buffer" . json-pretty-print-buffer))

          (with-eval-after-load 'eglot
            (setq-default eglot-workspace-configuration
                          '(:json (:format (:enable t) ;; doesn't work
                                   :validate (:enable t))))
            (add-to-list
             'eglot-server-programs
             '(((json-mode :language-id "json")
                (json-ts-mode :language-id "json")
                (jsonc-mode :language-id "jsonc")) .
               ("${pkgs.vscode-langservers-extracted}/bin/vscode-json-language-server" "--stdio"))))
        '';
        elispPackages = with pkgs.emacsPackages; [
          json-mode
          json5-ts-mode
          (treesit-grammars.with-grammars (
            grammars: with grammars; [
              tree-sitter-json
              tree-sitter-json5
            ]
          ))
        ];
      };
    };
}
