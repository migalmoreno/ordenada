{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "lisp";
  options =
    { pkgs, ... }:
    let
      inherit (lib) mkOption mkPackageOption types;
      defaultCustomSlyPrompt = # lisp
        ''
          (lambda (_p nickname error-level next-idx _c)
               (let ((dir (propertize (abbreviate-file-name default-directory)
                                      'font-lock-face 'diff-mode))
                     (nick (propertize nickname 'font-lock-face 'sly-mode-line))
                     (idx (propertize (number-to-string next-idx)
                                      'font-lock-face 'diff-mode))
                     (err-level (when (cl-plusp error-level)
                                  (concat (sly-make-action-button
                                           (format " [%d]" error-level)
                                           'sly-db-pop-to-debugger-maybe)
                                          ""))))
                 (concat "(" dir ")\n"
                         (propertize "<" 'font-lock-face 'sly-mrepl-prompt-face)
                         idx
                         (propertize ":" 'font-lock-face 'sly-mrepl-prompt-face)
                         nick
                         err-level
                         (propertize "> " 'font-lock-face 'sly-mrepl-prompt-face))))
        '';
    in
    {
      package = mkPackageOption pkgs "sbcl" { };
      extraPackages = mkOption {
        type = types.listOf types.package;
        description = "List of extra globally installed Lisp packages.";
        default = [ ];
      };
      customSlyPrompt = mkOption {
        type = types.nullOr types.str;
        description = ''
          An Emacs Lisp function that represents the custom Sly prompt. See
          the sly-mrepl-default-prompt documentation for information on its
          arguments and return value.
        '';
        default = defaultCustomSlyPrompt;
      };
      extraSourceRegistryFiles = mkOption {
        type = types.attrs;
        description = ''
          A set of files that will be added under
          ~/.config/common-lisp/source-registry.conf.d to allow the ASDF source
          registry mechanism to discover new Lisp systems in custom file-system
          locations.

          For example:

          extraSourceRegistryFiles."10-projects.conf".text = "(:tree "/home/user/projects/")";
        '';
        default = { };
      };
      extraSbclConfig = mkOption {
        type = types.lines;
        description = "Extra configuration to be put in ~/.sbclrc.";
        default = '''';
      };
      slynk = mkPackageOption pkgs [ "emacsPackages" "slynk" ] { };
      extraSlynkConfig = mkOption {
        type = types.lines;
        description = "Extra configuration to be put in in ~/.slynk.lisp";
        default = '''';
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      xdg.configFile = lib.mapAttrs' (
        name: value: lib.nameValuePair ("common-lisp/source-registry.conf.d/" + name) value
      ) config.ordenada.features.lisp.extraSourceRegistryFiles;
      home.packages = config.ordenada.features.lisp.extraPackages;
      home.file."slynk.lisp".text = # lisp
        ''
          (setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)
          ${config.ordenada.features.lisp.extraSlynkConfig}
        '';
      home.file.".sbclrc.lisp".text = # lisp
        ''
          (require :asdf)

          (let ((profile
                 (merge-pathnames "${config.home.profileDirectory}/lib/")))
            (when (and (probe-file profile)
                       (ignore-errors (asdf:load-system "cffi")))
              (push profile
                    (symbol-value
                     (find-symbol (string '*foreign-library-directories*)
                                  (find-package 'cffi))))))

          (let ((quicklisp-init
                 (merge-pathnames ".local/share/quicklisp/setup.lisp"
                                  (user-homedir-pathname))))
            (when (probe-file quicklisp-init)
              (load quicklisp-init)))

          ${config.ordenada.features.lisp.extraSbclConfig}
        '';

      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-lisp";
        config =
          with config.ordenada.features.lisp; # elisp
          ''
            (eval-when-compile
              (require 'sly))

            (defun ordenada-lisp-sly-autoconnect ()
              "Start a SLY REPL unless an active connection is already present."
              (unless (sly-connected-p)
                (save-excursion (sly))))

            (defun ordenada-lisp-setup-sly-history ()
              "Create an empty history file for SLY if missing."
              (unless (file-exists-p sly-mrepl-history-file-name)
                (make-empty-file sly-mrepl-history-file-name)))

            (add-hook 'debugger-mode-hook #'toggle-truncate-lines)
            (add-hook 'sly-mode-hook #'ordenada-lisp-sly-autoconnect)
            (add-hook 'sly-mrepl-mode-hook 'ordenada-lisp-setup-sly-history)
            (add-to-list 'display-buffer-alist
                         `(,(rx "*sly-mrepl" (* any) "*")
                           (display-buffer-no-window)
                           (allow-no-window . t)))

            (with-eval-after-load 'sly
              (sly-setup)
              (setq sly-words-of-encouragement '(""))
              (setq sly-command-switch-to-existing-lisp 'always)
              (setq sly-description-autofocus t)
              (setq sly-net-coding-system 'utf-8-unix)
              (setq sly-connection-poll-interval 0.1)
              (setq sly-enable-evaluate-in-emacs t)
              (setq sly-keep-buffers-on-connection-close nil))

            (with-eval-after-load 'sly-mrepl
              (require 'xdg)
              (let ((map sly-mode-map))
                (keymap-set map "C-c C-b" #'sly-eval-buffer)
                (keymap-set map "C-c C-q" #'sly-interrupt))
              (let ((map sly-mrepl-mode-map))
                (keymap-set map "C-M-q" #'indent-sexp)
                (keymap-set map "C-c C-z" #'sly-switch-to-most-recent)
                (keymap-set map "C-c M-n" #'sly-mrepl-next-prompt)
                (keymap-set map "C-c M-p" #'sly-mrepl-previous-prompt))
              (setq sly-mrepl-history-file-name
                    (expand-file-name "emacs/sly-mrepl-history"
                                      (xdg-cache-home)))
              (setq sly-mrepl-prevent-duplicate-history t)
              (setq sly-mrepl-pop-sylvester nil)

              ${
                lib.optionalString (customSlyPrompt != null) ''
                  (setq sly-mrepl-custom-prompt ${customSlyPrompt})
                ''
              })

              ${lib.optionalString config.ordenada.features.emacs.org.enable ''
                (with-eval-after-load 'org
                  (require 'ob-lisp)
                  (add-to-list 'org-structure-template-alist '("li" . "src lisp")))
                (with-eval-after-load 'ob-lisp
                    (setq org-babel-lisp-eval-fn 'sly-eval))
                (with-eval-after-load 'ob-core
                  (setq org-babel-default-header-args:lisp '((:results . "scalar"))))
              ''}

              (with-eval-after-load 'lisp-mode
                (keymap-set lisp-mode-map "C-c C-z" #'sly-mrepl)
                (setq inferior-lisp-program "${lib.getExe package}"))
          '';
        elispPackages = with pkgs.emacsPackages; [
          sly
          sly-asdf
        ];
      };
    };
}
