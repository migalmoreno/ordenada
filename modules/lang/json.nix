{
  lib,
  mkFeature,
  ordenada-lib,
  pkgs,
  ...
}:

## TODO: - apheleia integration
##       - schemas

mkFeature {
  name = "json";
  options = {
    schemas = lib.mkOption {
      type = lib.types.attrs;
      description = "List of schemas for specific file patterns.";
      default = {};
      example = lib.literalExpression ''
        let
          fooSchema = ./fooSchema.json;
        in
        [{ files: "*.foo.json": fooSchema }]'';
    };
  };
  homeManager =
    { config, pkgs, ... }:
    let
      schemaString = lib.concatStringsSep " " (
        lib.mapAttrsToList (
          name: value: ''("${name}" "${toString value}")''
        ) config.ordenada.features.json.schemas
      );
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
          (require 'cl-lib)
          (require 'json5-ts-mode)

          ;; setup
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

          ;; flymake
          ;; TODO: Move into own package
          (defvar-local ordenada-json--flymake-proc nil)

          (defun ordenada-json-flymake (report-fn &rest _args)
            (when (process-live-p ordenada-json--flymake-proc)
              (kill-process ordenada-json--flymake-proc))

            (let* ((source (current-buffer))
                   (mode (with-current-buffer source
                           (let* ((filename (buffer-file-name))
                                  (ext (when filename (file-name-extension filename))))
                           (cond
                             ((string-equal ext "json5") "json5")
                             ((string-equal ext "jsonc") "cjson")
                             (t "json"))))))
              (save-restriction
                (widen)
                (setq
                ordenada-json--flymake-proc
                (make-process
                   :name "ordenada-json-flymake"
                   :noquery t
                   :connection-type 'pipe
                   :buffer (generate-new-buffer " *json-flymake*")
                   :command `("${prantlf-jsonlint}/bin/jsonlint" "-c" "-q" "-M" ,mode)
                   :sentinel
                   (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (unwind-protect
                         (if (with-current-buffer source (eq proc ordenada-json--flymake-proc))
                           (with-current-buffer (process-buffer proc)
                             (goto-char (point-min))
                             (cl-loop
                               while (search-forward-regexp
                                       "^\\([^:]+\\): line \\([0-9]+\\), col \\([0-9]+\\), \\(.+?\\)\\.$"
                                       nil t)
                               for (beg . end) = (flymake-diag-region
                                                   source
                                                   (string-to-number (match-string 2))
                                                   (string-to-number (match-string 3)))
                               for msg = (match-string 4)
                               for type = :error
                               when (and beg end)
                               collect (flymake-make-diagnostic source
                                                                beg
                                                                end
                                                                type
                                                                msg)
                               into diags
                               finally (funcall report-fn diags)))
                             (flymake-log :warning "Canceling obsolete check %s"
                                     proc))
                         (kill-buffer (process-buffer proc)))))))
          (process-send-region ordenada-json--flymake-proc (point-min) (point-max))
          (process-send-eof ordenada-json--flymake-proc))))

          (defun ordenada-json--flymake-setup-backend ()
            (add-hook 'flymake-diagnostic-functions 'ordenada-json-flymake nil t))

          (defun ordenada-json-format-buffer (&optional buffer)
            "Format BUFFER using the `jsonlint' external command.

          This function formats the specified BUFFER, which defaults to
          the `current-buffer' if not provided.

          It works for `json', `jsonc' and `json5' files."
            (interactive)
            (let* ((buf (if (stringp buffer)
                            (get-buffer buffer)
                          (or buffer (current-buffer))))
                   (filename (with-current-buffer buf (buffer-file-name)))
                   (mode (with-current-buffer buf
                           (let* ((ext (when filename (file-name-extension filename))))
                             (cond
                              ((string-equal ext "json5") "json5")
                              ((string-equal ext "jsonc") "cjson")
                              (t "json")))))
                   (schema (when filename
                             (let ((matched
                                     (cl-find-if (lambda (pair)
                                                   (string-match-p
                                                     (wildcard-to-regexp (car pair))
                                                     (file-name-nondirectory filename)))
                                                  '(${schemaString}))))
                               (cadr matched))))
                   (args (append `("-p" "-M" ,mode)
                                 (if (stringp schema)
                                   `("-V" ,schema)
                                   '()))))
              (with-current-buffer buf
                (let ((orig-point (point)))
                  (save-excursion
                    (save-restriction
                      (widen)
                      ;(message args)
                      (apply
                        #'call-process-region
                        (point-min) (point-max)
                        "${prantlf-jsonlint}/bin/jsonlint"
                        t
                        t
                        nil
                        args)
                      (set-buffer-modified-p t)))
                  (goto-char (min (max orig-point (point-min))
                                  (point-max)))))))

          ;; minor modes
          (define-minor-mode ordenada-json-mode
            "Set up convenient tweaks for JSON/JSONC development."
            :group 'ordenada-json :keymap ordenada-json-mode-map
            (when ordenada-json-mode
              ;; (eglot-ensure)
              (ordenada-json--setup-electric-pairs-for-json)
              (add-hook 'flymake-diagnostic-functions 'ordenada-json-flymake nil t)
              (flymake-mode 1)))
          (mapcar (lambda (hook)
                    (add-hook (intern (concat (symbol-name hook) "-hook")) 'ordenada-json-mode))
                  '(json-mode json-ts-mode js-json-mode jsonc-mode))

          (define-minor-mode ordenada-json5-mode
            "Set up convenient tweaks for JSON5 development."
            :group 'ordenada-json :keymap ordenada-json5-mode-map
            (when ordenada-json5-mode
              (setq json5-ts-mode-indent-offset 2)
              (ordenada-json--setup-electric-pairs-for-json)
              (add-hook 'flymake-diagnostic-functions 'ordenada-json-flymake nil t)
              (flymake-mode 1)
            ))
          (add-hook 'json5-ts-mode-hook 'ordenada-json5-mode)

          (keymap-set ordenada-json-mode-map "C-c f"
                      '("Format buffer" . ordenada-json-format-buffer))
          (keymap-set ordenada-json5-mode-map "C-c f"
                      '("Format buffer" . ordenada-json-format-buffer))

          ;; ;; eglot
          ;; (with-eval-after-load 'eglot
          ;;   (setq-default eglot-workspace-configuration
          ;;                 '(:json (:format (:enable t) ;; doesn't work
          ;;                          :validate (:enable t))))
          ;;   (add-to-list
          ;;    'eglot-server-programs
          ;;    '(((jsonc-mode :language-id "jsonc") ;; needs to come first
          ;;       (json-mode :language-id "json")
          ;;       (json-ts-mode :language-id "json")) .
          ;;      ("${pkgs.vscode-langservers-extracted}/bin/vscode-json-language-server" "--stdio"))))
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
