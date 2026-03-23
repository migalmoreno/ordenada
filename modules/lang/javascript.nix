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
  name = "javascript";
  options =
    { pkgs, ... }:
    {
      node = lib.mkPackageOption pkgs "nodejs" { };
      bun = lib.mkPackageOption pkgs "bun" { };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      home.packages =
        with config.ordenada.features.javascript;
        [
          node
          bun
        ]
        ++ (with pkgs; [
          (yarn.override { nodejs = null; })
          nodePackages.prettier
        ]);

      ordenada.features.emacs.corfu.globalModes = mkIf (config.ordenada.features.emacs.corfu.enable) [
        "js-ts-mode"
        "jsx-ts-mode"
        "typescript-ts-mode"
        "tsx-ts-mode"
        "css-ts-mode"
        "web-mode"
      ];

      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-javascript";
        config = # elisp
          ''
            (defgroup ordenada-javascript nil
              "General JavaScript/TypeScript programming utilities."
              :group 'ordenada)

            (defvar ordenada-javascript-mode-map (make-sparse-keymap))
            (defvar ordenada-javascript-nodejs-repl-mode-command-map nil
              "Map to bind `nodejs-repl' commands under.")
            (defvar ordenada-javascript-jtsx-command-map nil
              "Map to bind `jtsx' commands under.")

            (define-prefix-command 'ordenada-javascript-nodejs-repl-mode-command-map)
            (define-prefix-command 'ordenada-javascript-jtsx-command-map)

            (defun ordenada-javascript--next-line-function-p ()
              "Return t if the next non-empty line contains a JavaScript function declaration.
Falls back to text matching if tree-sitter parsing is incomplete."
              (interactive)
              (save-match-data
              (save-excursion
                (forward-line 1)
                ;; Skip blank lines
                (while (and (not (eobp))
                            (looking-at-p "^[[:space:]]*$"))
                  (forward-line 1))
                (unless (eobp)
                (back-to-indentation)
                  (or
                   (ordenada-javascript--check-function-with-treesit)
                   ;; Fallback to regex if tree-sitter has errors
                   (ordenada-javascript--check-function-with-regex))))))

              (defun ordenada-javascript--check-function-with-treesit ()
                "Check for function signature using tree-sitter."
                (when-let ((node (treesit-node-at (point))))
                  (let ((current node)
                        (found nil)
                        (has-error nil))
                    ;; Check if we're in an error state
                    (while current
                      (when (string= (treesit-node-type current) "ERROR")
                        (setq has-error t))
                      (setq current (treesit-node-parent current)))
                    ;; Only use tree-sitter if no errors
                    (unless has-error
                      (setq current node)
                      (let ((line-start (line-beginning-position)))
                        (while (and current (not found))
                          (when (member (treesit-node-type current)
                                        '("function_declaration"
                                          "arrow_function"
                                          "function_expression"
                                          "method_definition"
                                          "generator_function_declaration"
                                          "async_function_declaration"))
                            (when (= (line-number-at-pos (treesit-node-start current))
                                     (line-number-at-pos line-start))
                              (setq found t)))
                          (setq current (treesit-node-parent current)))))
                    found)))

              (defun ordenada-javascript--check-function-with-regex ()
                "Check for function signature using regex patterns."
                (looking-at
                 (rx (zero-or-more space)
                     (optional "export" (one-or-more space))
                     (optional (or "async" "static") (one-or-more space))
                     (or
                      ;; function declaration: function foo() or function* foo()
                      (seq "function" (optional "*"))
                      ;; const/let/var with arrow or function
                      (seq (or "const" "let" "var") (one-or-more space)
                           (one-or-more (or alphanumeric "_" "$"))
                           (zero-or-more space) "=" (zero-or-more space)
                           (optional "async" (one-or-more space))
                           (or "(" "function"))))))

            (defun ordenada-javascript-jsdoc-or-line-break ()
              "Inserts JSDoc at point if line matches `/**'. Otherwise executes `js2-line-break'
              at given point."
              (interactive)
              (let ((p (point)))
                (if (and (looking-back "/\\*\\*" 3)
                         (ordenada-javascript--next-line-function-p))
                    (progn
                      (beginning-of-line)
                      (kill-line)
                      (next-line)
                      (message "%s" (thing-at-point 'line t))
                      (jsdoc)
                      (goto-char (search-backward-regexp "^/\\*\\*$"))
                      (beginning-of-line)
                      (backward-delete-char 1)
                      (next-line)
                      (end-of-line))
                  (progn
                    (goto-char p)
                    (if (string-match-p "comment" (treesit-node-type (treesit-node-at p 'javascript)))
                        (funcall 'js2-line-break)
                      (funcall 'newline-and-indent))))))

            (defun ordenada-javascript--disable-eglot-parts ()
              (setq-local eglot-stay-out-of '(flymake)))

            (defun ordenada-javascript--setup-flymake-for-eglot ()
              (flymake-mode t)
              (when (derived-mode-p 'typescript-ts-mode 'js-ts-mode 'tsx-ts-mode 'jsx-ts-mode)
                (flymake-eslint-enable))
              (add-to-list 'mode-line-misc-info `(flymake-mode (" " flymake-mode-line-counters " "))))

            (defun ordenada-javascript--eglot-code-action-missing-imports (beg &optional end)
              "Executes `source.addMissingImports.ts' between BEG and END."
              (interactive (eglot--code-action-bounds))
              (eglot-code-actions beg end "source.addMissingImports.ts" t))

            (defun ordenada-javascript--eglot-code-action-unused-imports (beg &optional end)
              "Executes `source.removeUnusedImports.ts' between BEG and END."
              (interactive (eglot--code-action-bounds))
              (eglot-code-actions beg end "source.removeUnusedImports.ts" t))

            (add-to-list 'major-mode-remap-alist '(javascript-mode . jtsx-jsx-mode))
            (add-to-list 'major-mode-remap-alist '(typescript-mode . jtsx-tsx-mode))
            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
            (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jtsx-jsx-mode))
            (add-to-list 'auto-mode-alist '("\\.ts\\'" . jtsx-typescript-mode))
            (add-to-list 'auto-mode-alist '("\\(\\.[c|m]js[m]?\\|\\.har\\)\\'" . jtsx-jsx-mode) t)
            (add-to-list 'auto-mode-alist '("\\(\\.js[mx]?\\|\\.har\\)\\'" . jtsx-jsx-mode) t)
            (define-derived-mode jsx-ts-mode tsx-ts-mode "JavaScript[JSX]")

            (define-minor-mode ordenada-javascript-mode
              "Set up convenient tweaks for JavaScript/TypeScript development."
              :group 'ordenada-javascript :keymap ordenada-javascript-mode-map
              (when ordenada-javascript-mode
                (setq tab-width 2)

                (ordenada-javascript--disable-eglot-parts)

                (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
                (add-hook 'js2-mode-hook #'js2-refactor-mode)

                (local-unset-key (kbd "RET"))
                (local-set-key (kbd "RET") 'ordenada-javascript-jsdoc-or-line-break)

                (eglot-ensure)
                (js2-minor-mode)
                (js2-imenu-extras-mode)
                (js2-refactor-mode)
                (npm-mode)))

            (let ((map ordenada-javascript-nodejs-repl-mode-command-map))
              (keymap-set map "e" '("Send last expression" . nodejs-repl-send-last-expression))
              (keymap-set map "j" '("Send line" . nodejs-repl-send-line))
              (keymap-set map "r" '("Send region" . nodejs-repl-send-region))
              (keymap-set map "C-c" '("Send buffer" . nodejs-repl-send-buffer))
              (keymap-set map "C-l" '("Load file" . nodejs-repl-load-file))
              (keymap-set map "C-z" '("Switch to REPL" . nodejs-repl-switch-to-repl)))
            (let ((map ordenada-javascript-jtsx-command-map))
              (keymap-set map "j" '("Jump to element boundary" . jtsx-jump-jsx-element-tag-dwim))
              (keymap-set map "r" '("Rename element" . jtsx-rename-jsx-element))
              (keymap-set map "<down>" '("Move element forward" . jtsx-move-jsx-element-forward))
              (keymap-set map "<up>" '("Move element backward" . jtsx-move-jsx-element-backward))
              (keymap-set map "<right>" '("Move element in forward" . jtsx-move-jsx-element-step-in-forward))
              (keymap-set map "<left>" '("Move element in backward" . jtsx-move-jsx-element-step-in-backward))
              (keymap-set map "C-<down>" '("Move tag forward" . jtsx-move-jsx-element-tag-forward))
              (keymap-set map "C-<up>" '("Move tag backward" . jtsx-move-jsx-element-tag-backward))
              (keymap-set map "w" '("Wrap in JSX element" . jtsx-wrap-in-jsx-element))
              (keymap-set map "W" '("Unwrap JSX element" . jtsx-unwrap-jsx))
              (keymap-set map "d" '("Delete JSX Element attribute" . jtsx-delete-jsx-attribute))
              (keymap-set map "D" '("Delete JSX Element" . jtsx-delete-jsx-node))
              (keymap-set map "t" '("Toggle attribute orientation" . jtsx-toggle-jsx-attributes-orientation)))

            (keymap-set ordenada-javascript-mode-map "C-c N"
                        '("Node REPL" . ordenada-javascript-nodejs-repl-mode-command-map))
            (keymap-set ordenada-javascript-mode-map "C-c j"
                        '("JSX/TSX" . ordenada-javascript-jtsx-command-map))
            (keymap-set ordenada-javascript-mode-map "C-c f"
                        '("Format buffer" . eslint-fix))

            (with-eval-after-load 'eglot
              (keymap-set ordenada-javascript-mode-map "C-c c i"
                          '("Add missing imports" . ordenada-javascript--eglot-code-action-missing-imports))
              (keymap-set ordenada-javascript-mode-map "C-c c I"
                          '("Remove unused imports" . ordenada-javascript--eglot-code-action-unused-imports)))

            (mapcar (lambda (hook)
                      (add-hook (intern (concat (symbol-name hook) "-hook")) 'ordenada-javascript-mode))
                    '(js-ts-mode typescript-ts-mode tsx-ts-mode jsx-ts-mode))

            (with-eval-after-load 'nodejs-repl
              (setopt nodejs-repl-command "${pkgs.nodejs}/bin/node"))

            (with-eval-after-load 'flymake-eslint
              (setopt flymake-eslint-executable-name "${pkgs.nodePackages_latest.eslint}/bin/eslint"))

            (with-eval-after-load 'eslint-fix
              (setopt eslint-fix-executable "${pkgs.nodePackages_latest.eslint}/bin/eslint"))

            (add-hook 'eglot-managed-mode-hook 'ordenada-javascript--setup-flymake-for-eglot)
            (with-eval-after-load 'eglot
              (add-to-list
               'eglot-server-programs
               '(((jsx-ts-mode :language-id "javascriptreact")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")) .
                  ("${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server" "--stdio"
                   :initializationOptions
                   (:tsserver (:path "${pkgs.nodePackages.typescript}/lib/node_modules/typescript/lib"))))))

            (with-eval-after-load 'npm-mode
              (fset 'npm-mode-command-keymap npm-mode-command-keymap)
              (keymap-set npm-mode-keymap "C-c n" '("NPM" . npm-mode-command-keymap)))

            (with-eval-after-load 'js2-refactor
              (setopt js2r-prefer-let-over-var t)
              (setopt js2r-prefered-quote-type 2)
              (js2r-add-keybindings-with-prefix "C-c r"))

            (with-eval-after-load 'js
              (setopt js-indent-level 2)
              (setopt js-chain-indent t))

            (with-eval-after-load 'org
              (add-to-list 'org-structure-template-alist '("js" . "src js")))
            (with-eval-after-load 'ob-core
              (require 'ob-js))

            (with-eval-after-load 'js2-mode
              (setopt js2-basic-offset 2)
              (setopt js2-skip-preprocessor-directives t)
              (setopt js2-mode-show-parse-errors nil)
              (setopt js2-mode-show-strict-warnings nil)
              (setopt js2-strict-missing-semi-warning nil)
              (setopt js2-highlight-level 3)
              (setopt js2-idle-timer-delay 0.15))

            (with-eval-after-load 'web-mode
              (setopt web-mode-markup-indent-offset 2)
              (setopt web-mode-css-indent-offset 2)
              (setopt web-mode-code-indent-offset 2))

            (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
            (with-eval-after-load 'css-mode
            (setopt css-indent-offset 2))
          '';
        elispPackages = with pkgs.emacsPackages; [
          eslint-fix
          flymake-eslint
          js2-mode
          js2-refactor
          jtsx
          jsdoc
          npm-mode
          nodejs-repl
          (treesit-grammars.with-grammars (
            grammars: with grammars; [
              tree-sitter-css
              tree-sitter-javascript
              tree-sitter-tsx
              tree-sitter-typescript
              tree-sitter-jsdoc
            ]
          ))
          web-mode
          markdown-mode # needed for preview with eldoc
        ];
      };
    };
}
