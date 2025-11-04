{
  lib,
  mkFeature,
  ordenada-lib,
  pkgs,
  ...
}:

## TODO: - apheleia integration
##       - json5 validation/format (v-jsonlint?)

mkFeature {
  name = "json";
  homeManager =
    { config, pkgs, ... }:
    let
      prantlf-jsonlint =
        let
          lockfile = ./../../packages/prantlf-jsonlint/package-lock.json;
        in
        pkgs.buildNpmPackage rec {
          pname = "jsonlint";
          version = "16.0.0";

          src = pkgs.fetchFromGitHub {
            owner = "prantlf";
            repo = "jsonlint";
            rev = "v16.0.0";
            hash = "sha256-pXTw9U8E/Xocy4+M9P3hQjZxvUq/iAHhxwnrJA3hLK0=";
          };

          postPatch = ''
            cp ${lockfile} package-lock.json
          '';

          npmInstallFlags = [ "--omit=dev" ];

          buildPhase = ''
            ${pkgs.coreutils-full}/bin/cat \
              src/prefix.js.txt \
              src/unicode.js \
              src/custom-parser.js \
              src/pointer.js \
              src/native-parser.js \
              src/configurable-parser.js \
              src/suffix.js.txt \
              > lib/jsonlint.js
          '';

          npmDepsHash = "sha256-EIghZWuPvynS+EnhCSPe/PnWF0QjC0csX3Kg2ClQnOU=";

          meta = with lib; {
            description = "JSON/CJSON/JSON5 parser, syntax & schema validator and pretty-printer with a command-line client, written in pure JavaScript. ";
            homepage = "https://prantlf.github.io/jsonlint/";
            license = licenses.mit;
          };
        };
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
        prantlf-jsonlint
      ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-json";
        config = ''
          (require 'json5-ts-mode)

          (defgroup ordenada-json nil
            "General JSON/JSON5 programming utilities."
            :group 'ordenada)

          (defvar ordenada-json-mode-map (make-sparse-keymap))
          (defvar ordenada-json5-mode-map (make-sparse-keymap))

          (defun ordenada-json--setup-electric-pairs-for-json ()
            (electric-pair-local-mode)
            (setq-local electric-pair-pairs
              (append electric-pair-pairs '((91 . 93)
                                            (123 . 125))))
            (setq-local electric-pair-text-pairs electric-pair-pairs))

          (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
          (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.jsonc\\'" . jsonc-mode))
          (add-to-list 'auto-mode-alist '("\\.json5\\'" . json5-ts-mode))

          (define-minor-mode ordenada-json-mode
            "Set up convenient tweaks for JSON/JSONC development."
            :group 'ordenada-json :keymap ordenada-json-mode-map
            (when ordenada-json-mode
              (eglot-ensure)
              (ordenada-json--setup-electric-pairs-for-json)))
          (mapcar (lambda (hook)
                    (add-hook (intern (concat (symbol-name hook) "-hook")) 'ordenada-json-mode))
                  '(json-mode json-ts-mode js-json-mode jsonc-mode))

          (define-minor-mode ordenada-json5-mode
            "Set up convenient tweaks for JSON5 development."
            :group 'ordenada-json :keymap ordenada-json5-mode-map
            (when ordenada-json5-mode
              (ordenada-json--setup-electric-pairs-for-json)
              (setq json5-ts-mode-indent-offset 2)))
          (add-hook 'json5-ts-mode-hook 'ordenada-json5-mode)

          (keymap-set ordenada-json-mode-map "C-c f"
                      '("Format buffer" . json-pretty-print-buffer))

          (with-eval-after-load 'eglot
            (setq-default eglot-workspace-configuration
                          '(:json (:format (:enable t) ;; doesn't work
                                   :validate (:enable t))))
            (add-to-list
             'eglot-server-programs
             '(((jsonc-mode :language-id "jsonc") ;; needs to come first
                (json-mode :language-id "json")
                (json-ts-mode :language-id "json")) .
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
