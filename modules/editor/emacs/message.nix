{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkEnableOption;
in
{
  options = {
    ordenada.features.emacs.message = {
      enable = mkEnableOption "the Emacs message feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.message" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-message";
        config = ''
          (eval-when-compile
            (require 'message)
            (require 'sendmail))

          (defconst ordenada-message-srht-patch-control-codes
           '("PROPOSED" "NEEDS_REVISION" "SUPERSEDED" "APPROVED" "REJECTED"
             "APPLIED")
           "Control codes for SourceHut patches.  See
          `ordenada-message-srht-add-email-control-code' for how to apply them.")

          (defun ordenada-message-add-srht-email-control-code (control-code)
            "Add a custom header for SourceHut email controls.  The CONTROL-CODE
          is among `ordenada-message-srht-patch-control-codes'."
            (interactive
             (list (completing-read "Select control code: "
                                    ordenada-message-srht-patch-control-codes
                                    nil t)))
            (if (member control-code ordenada-message-srht-patch-control-codes)
                (unless (message-fetch-field "X-Sourcehut-Patchset-Update")
                  (message-add-header (format "X-Sourcehut-Patchset-Update: %s"
                                              control-code)))
                (user-error "%s is not specified in
          `ordenada-notmuch-patch-control-codes'" control-code)))
          ${mkIf (hasFeature "emacs.gnus" user) ''
            (defun rde-message-add-gcc-header ()
              "Prompt for a Gcc header from `rde-gnus-topic-alist'.
            This will allow a message to be stored in the right directory
            of the IMAP server (usually \"Sent\").
            If this header is missing, the outgoing message will go through,
            but it won't appear on the right Maildir directory."
               (if (gnus-alive-p)
                   (unless (message-fetch-field "Gcc")
                     (message-add-header
                      (format "Gcc: %s"
                              (let ((groups
                                     (cl-remove-if-not
                                      (lambda (group)
                                        (string-match (rx "Sent" eol) group))
                                      (rde-gnus--get-topic-groups))))
                                (if (> 1 (length groups))
                                    (completing-read "Account: " groups)
                                  (car groups))))))
                 (error "Gnus is not running.  No GCC header will be inserted")))
          ''}
          (with-eval-after-load 'message
            (setopt message-hidden-headers '())
            (setopt message-kill-buffer-on-exit t)
            (setopt message-signature
                    "${user.features.mail.defaultMessageSignature}")
            (setopt message-citation-line-function
                    #'message-insert-formatted-citation-line)
            (setopt message-citation-line-format "On %Y-%m-%d %R, %N wrote:\n")
            (setopt message-auto-save-directory
                    (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                            "/emacs/mail-drafts"))
          ${mkIf (hasFeature "mail.msmtp" user) ''
            (setopt sendmail-program
                    "${user.features.mail.msmtp.package}/bin/msmtp")
            (setopt message-send-mail-function
                    #'message-send-mail-with-sendmail)
            (setopt message-sendmail-f-is-evil t)
            (setopt message-sendmail-extra-arguments
                    '("--read-envelope-from"))
          ''}
          ${
            mkIf (user.features.userInfo.gpgPrimaryKey != null) ''
              (setq mml-secure-openpgp-signers
                    '("${user.features.userInfo.gpgPrimaryKey}"))
              (add-hook '${mkIf (hasFeature "emacs.gnus" user) "gnus-"}message-setup-hook
                        #'mml-secure-message-sign-pgpmime)
            ''
          })
        '';
      };
    });
  };
}
