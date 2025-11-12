{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "nyxt";
  options =
    { pkgs, ... }:
    let
      inherit (lib)
        mkEnableOption
        mkOption
        mkPackageOption
        types
        ;
    in
    {
      package = mkPackageOption pkgs "nyxt" { };
      defaultBrowser = mkEnableOption "using Nyxt as your default browser";
      autoStartSlynk = mkEnableOption "connecting to Nyxt remotely via a Lisp REPL";
      startupFlags = mkOption {
        type = types.listOf types.str;
      };
      scrollDistance = mkOption {
        type = types.int;
        description = "Scroll distance in number of lines.";
        default = 50;
      };
      extraConfig = mkOption {
        type = types.lines;
        description = ''
          Extra Nyxt configuration. Consult Nyxt's manual page
          (accessible via the command `manual`) to discover more functionalities.
        '';
        default = '''';
      };
      defaultCookiePolicy = mkOption {
        type = types.enum [
          "always"
          "never"
          "no-third-party"
        ];
        description = ''
          The default cookie policy. One of:
          - `always` (accept all cookies)
          - `never` (reject all cookies)
          - `no-third-party` (only accept the current site's cookies)
        '';
        default = "no-third-party";
      };
      downloadEngine = mkOption {
        type = types.enum [ "renderer" ];
        description = "The engine to use for browser downloads.";
        default = "renderer";
      };
      defaultNewBufferUrl = mkOption {
        type = types.nullOr types.str;
        description = ''
          The default new page URL you'll be prompted with at browser startup
          if restoreSession is false, otherwise you'll be shown the last-accessed
          page.
        '';
        default = null;
      };
      restoreSession = mkEnableOption "restoring the previous browser session on startup";
      temporaryHistory = mkEnableOption "recording browser history in `nyxt-temporary-directory` (by default /tmp)";
    };
  homeManager =
    { config, pkgs, ... }:
    lib.mkMerge [
      {
        programs.nyxt = with config.ordenada.features.nyxt; {
          inherit package;
          enable = true;
          config = extraConfig;
        };
      }
      (lib.mkIf config.ordenada.features.nyxt.defaultBrowser {
        home.sessionVariables."BROWSER" = lib.getExe config.ordenada.features.nyxt.package;
        xdg.mimeApps.defaultApplications = lib.genAttrs [
          "text/html"
          "text/xml"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/about"
        ] (lib.const "nyxt.desktop");
      })
      {
        programs.nyxt.config =
          with config.ordenada.features.nyxt; # lisp
          ''
            (let ((sbcl-init (merge-pathnames ".sbclrc" (user-homedir-pathname))))
              (when (probe-file sbcl-init)
                (load sbcl-init)))
            (asdf:ensure-source-registry)
            (use-nyxt-package-nicknames)

            (defvar *ordenada-keymap* (make-keymap "ordenada-map"))
            (define-key *ordenada-keymap*
              "S-b" #'switch-buffer
              "S-w" #'delete-current-buffer)

            (define-mode ordenada-keymap-mode ()
              "Dummy mode to apply keybindings in `*ordenada-keymap*.'"
              ((keyscheme-map
                (keymaps:make-keyscheme-map keyscheme:emacs *ordenada-keymap*))
               (visible-in-status-p nil)))

            (define-configuration nyxt/mode/hint:hint-mode
                ((nyxt/mode/hint:hints-alphabet "${
                  if config.ordenada.features.keyboard.layout.variant == "dvorak" then
                    "aoeuidhtns"
                  else
                    "asdfghjklqwertyuiop"
                }")))

            (define-configuration document-buffer
              ((smooth-scrolling t)
               (scroll-distance ${toString scrollDistance})))

            (define-configuration web-buffer
              ((default-modes (append '(ordenada-keymap-mode) %slot-value%))
               (download-engine :${downloadEngine})))

            (define-configuration browser
              ((default-cookie-policy :${defaultCookiePolicy})
               (restore-session-on-startup-p ${ordenada-lib.lisp.toBoolean restoreSession})
               ${
                 lib.optionalString (defaultNewBufferUrl != null) ''
                   (default-new-buffer-url (quri:uri ${defaultNewBufferUrl}))
                 ''
               }))

            ${lib.optionalString autoStartSlynk ''
              (unless nyxt::*run-from-repl-p*
                (start-slynk))
            ''}

            ${lib.optionalString temporaryHistory ''
              (define-class tmp-profile (nyxt-profile)
                ((files:name :initform "nyxt-tmp"))
                (:documentation "Temporary profile."))

              (defmethod files:resolve ((profile tmp-profile)
                                        (file history-file))
                "Store history in a temporary directory."
                (sera:path-join
                 (files:expand
                  (make-instance 'nyxt-temporary-directory))
                 (uiop:relativize-pathname-directory
                  (call-next-method))))

              (define-configuration web-buffer
                ((profile (make-instance
                           (or (find-profile-class
                                (getf *options* :profile))
                               'tmp-profile)))))

            ''}
          '';
      }
    ];
}
