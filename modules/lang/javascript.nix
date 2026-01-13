{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

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
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-javascript";
        ## TODO: `ordenada-javascript--next-line-function-or-arrow-p` is too
        ##       primitive, we should use tree-sitter to determine if next
        ##       line is a function. With this approach, a wrong function
        ##       definition, like `const () => 1` will cause bugs because
        ##       the predicate returns `t`.
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

            (defun ordenada-javascript--next-line-function-or-arrow-p ()
              "Return `t' if the next line contains `function' or `=>'."
              (save-excursion
                (forward-line 1)
                (let ((line (thing-at-point 'line t)))
                  (when line
                    (or (and (string-match-p "\\<function\\>" line) t)
                        (and (string-match-p "=>" line) t))))))

            (defun ordenada-javascript-jsdoc-or-line-break ()
              "Inserts JSDoc at point if line matches `/**'. Otherwise executes `js2-line-break'
              at given point."
              (interactive)
              (let ((p (point)))
                (beginning-of-line)
                (if (and (looking-at-p "^[[:blank:]]*/\\*\\*$")
                         (ordenada-javascript--next-line-function-or-arrow-p))
                  (progn
                    (kill-line)
                    (next-line)
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

            (defun ordenada-javascript--setup-electric-pairs-for-jsx-tsx ()
              (electric-pair-local-mode)
              (setq-local electric-pair-pairs (append electric-pair-pairs '((60 . 62))))
              (setq-local electric-pair-text-pairs electric-pair-pairs))

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
                (ordenada-javascript--setup-electric-pairs-for-jsx-tsx)

                (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
                (add-hook 'js2-mode-hook #'js2-refactor-mode)

                (eglot-ensure)
                (js2-minor-mode)
                (js2-imenu-extras-mode)
                (js2-refactor-mode)
                (npm-mode)))

            (let ((map ordenada-javascript-nodejs-repl-mode-command-map))
              (keymap-set map "e" #'nodejs-repl-send-last-expression)
              (keymap-set map "j" #'nodejs-repl-send-line)
              (keymap-set map "r" #'nodejs-repl-send-region)
              (keymap-set map "C-c" #'nodejs-repl-send-buffer)
              (keymap-set map "C-l" #'nodejs-repl-load-file)
              (keymap-set map "C-z" #'nodejs-repl-switch-to-repl))
            (let ((map ordenada-javascript-jtsx-command-map))
              (keymap-set map "j" #'jtsx-jump-jsx-element-tag-dwim)
              (keymap-set map "r" #'jtsx-rename-jsx-element)
              (keymap-set map "<down>" #'jtsx-move-jsx-element-forward)
              (keymap-set map "<up>" #'jtsx-move-jsx-element-backward)
              (keymap-set map "<right>" #'jtsx-move-jsx-element-step-in-forward)
              (keymap-set map "<left>" #'jtsx-move-jsx-element-step-in-backward)
              (keymap-set map "C-<down>" #'jtsx-move-jsx-element-tag-forward)
              (keymap-set map "C-<up>" #'jtsx-move-jsx-element-tag-backward)
              (keymap-set map "w" #'jtsx-wrap-in-jsx-element)
              (keymap-set map "W" #'jtsx-unwrap-jsx)
              (keymap-set map "d" #'jtsx-delete-jsx-attribute)
              (keymap-set map "D" #'jtsx-delete-jsx-node)
              (keymap-set map "t" #'jtsx-toggle-jsx-attributes-orientation))

            (keymap-set ordenada-javascript-mode-map "C-c N"
                        '("node repl" . ordenada-javascript-nodejs-repl-mode-command-map))
            (keymap-set ordenada-javascript-mode-map "C-c j"
                        '("j/tsx" . ordenada-javascript-jtsx-command-map))
            (keymap-set ordenada-javascript-mode-map "C-c f"
                        '("Format buffer" . eslint-fix))
            (keymap-set ordenada-javascript-mode-map "C-c c i"
                        #'ordenada-javascript--eglot-code-action-missing-imports)
            (keymap-set ordenada-javascript-mode-map "C-c c I"
                        #'ordenada-javascript--eglot-code-action-unused-imports)

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
              (keymap-set npm-mode-keymap "C-c n" '("npm" . npm-mode-command-keymap)))

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
