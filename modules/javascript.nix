{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options.ordenada.features.javascript = {
    enable = lib.mkEnableOption "the JavaScript feature";
  };
  config = {
    home-manager = mkHomeConfig config "javascript" (user: {
      home.packages = with pkgs; [
        jq
        nodejs
        yarn
        nodePackages.prettier
      ];
      programs.emacs = mkElispConfig {
        name = "ordenada-javascript";
        config = ''
          (defgroup ordenada-javascript nil
            "General JavaScript/TypeScript programming utilities."
            :group 'ordenada)

          (defvar ordenada-javascript-mode-map (make-sparse-keymap))
          (defvar ordenada-javascript-nodejs-repl-mode-command-map nil
            "Map to bind `nodejs-repl' commands under.")
          (define-prefix-command 'ordenada-javascript-nodejs-repl-mode-command-map)

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

          (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
          (add-to-list 'major-mode-remap-alist '(typescript-mode . tsx-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-ts-mode))
          (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
          (add-to-list 'auto-mode-alist '("\\(\\.[c|m]js[m]?\\|\\.har\\)\\'" . js-ts-mode) t)
          (define-derived-mode jsx-ts-mode tsx-ts-mode "JavaScript[JSX]")

          (define-minor-mode ordenada-javascript-mode
            "Set up convenient tweaks for JavaScript/TypeScript development."
            :group 'ordenada-javascript :keymap ordenada-javascript-mode-map
            (when ordenada-javascript-mode
              (ordenada-javascript--disable-eglot-parts)
              (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
              (eglot-ensure)
              (setq indent-tabs-mode nil)
              (ordenada-javascript--setup-electric-pairs-for-jsx-tsx)
              (js2-minor-mode)
              (js2-imenu-extras-mode)
              (npm-mode)))

          (let ((map ordenada-javascript-nodejs-repl-mode-command-map))
            (define-key map (kbd "e") 'nodejs-repl-send-last-expression)
            (define-key map (kbd "j") 'nodejs-repl-send-line)
            (define-key map (kbd "r") 'nodejs-repl-send-region)
            (define-key map (kbd "C-c") 'nodejs-repl-send-buffer)
            (define-key map (kbd "C-l") 'nodejs-repl-load-file)
            (define-key map (kbd "C-z") 'nodejs-repl-switch-to-repl))
          (define-key ordenada-javascript-mode-map (kbd "C-c C-r")
            '("repl" . ordenada-javascript-nodejs-repl-mode-command-map))
          (define-key ordenada-javascript-mode-map (kbd "C-c f")
            '("Format buffer" . eslint-fix))

          (mapcar (lambda (hook)
                    (add-hook (intern (concat (symbol-name hook) "-hook")) 'ordenada-javascript-mode))
                  '(js-ts-mode typescript-ts-mode tsx-ts-mode jsx-ts-mode))

          (with-eval-after-load 'nodejs-repl
            (setq nodejs-repl-command "${pkgs.nodejs}/bin/node"))

          (with-eval-after-load 'flymake-eslint
            (setq flymake-eslint-executable-name "${pkgs.nodePackages_latest.eslint}/bin/eslint"))

          (with-eval-after-load 'eslint-fix
            (setq eslint-fix-executable "${pkgs.nodePackages_latest.eslint}/bin/eslint"))

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
            (define-key npm-mode-keymap (kbd "C-c n")
              '("npm" . npm-mode-command-keymap)))

          (with-eval-after-load 'js
            (setq js-indent-level 2))

          (with-eval-after-load 'js2-mode
            (setq js-chain-indent t)
            (setq js2-basic-offset 2)
            (setq js2-skip-preprocessor-directives t)
            (setq js2-mode-show-parse-errors nil)
            (setq js2-mode-show-strict-warnings nil)
            (setq js2-strict-missing-semi-warning nil)
            (setq js2-highlight-level 3)
            (setq js2-idle-timer-delay 0.15))

          (with-eval-after-load 'web-mode
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2))

          (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
          (with-eval-after-load 'css-mode
            (setq css-indent-offset 2))
        '';
        elispPackages = with pkgs.emacsPackages; [
          eslint-fix
          flymake-eslint
          js2-mode
          json-mode
          npm-mode
          nodejs-repl
          (treesit-grammars.with-grammars (
            grammars: with grammars; [
              tree-sitter-css
              tree-sitter-javascript
              tree-sitter-json
              tree-sitter-tsx
              tree-sitter-typescript
            ]
          ))
          web-mode
        ];
      };
    });
  };
}
