{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.ordenada;
in
{
  options = {
    ordenada.features.emacs.gnus = {
      enable = mkEnableOption "the Emacs Gnus feature";
      postingStyles = mkOption {
        type = types.listOf types.str;
        description = "Posting styles based on the current group or article.";
        default = [ ];
      };
      topicGroups = mkOption {
        type = types.attrsOf (types.listOf types.str);
        description = "Topics-groups attrset to categorize groups into topics.";
        default = { };
      };
      topicTopology = mkOption {
        type = types.listOf types.str;
        description = "Hierarchy of topics.";
        default = [ ];
      };
      groupParameters = mkOption {
        type = with types; attrsOf (attrsOf (either str int));
        description = "Group-specific settings.";
        default = { };
      };
      directory = mkOption {
        type = types.str;
        default = "${cfg.features.xdg.baseDirs.cacheHome}/emacs/gnus";
        description = "Gnus directory variable.";
      };
      messageArchiveMethod = mkOption {
        type = types.nullOr (types.listOf types.str);
        description = "Mail method to archive the messages you've sent.";
        default = null;
      };
      messageArchiveGroup = mkOption {
        type = types.nullOr (types.listOf types.str);
        description = "Group name where to save the messages you've written.";
        default = null;
      };
      key = mkOption {
        type = types.str;
        description = "Keybinding to launch the Gnus newsgroup reader.";
        default = "g";
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.gnus" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-gnus";
        config =
          with user.features.emacs.gnus;
          with config.home-manager.users.${user.name};
          ''
            (defgroup ordenada-gnus nil
              "Customizations for the Gnus newsreader."
              :group 'ordenada)
            (defcustom ordenada-gnus-topic-topology nil
              "Topics topology for Gnus."
              :group 'ordenada-gnus
              :type 'list)
            (defcustom ordenada-gnus-topic-alist nil
              "Alist of Gnus topics."
              :group 'ordenada-gnus
              :type 'list)
            (defvar ordenada-gnus-subscribed-p nil
              "Whether we're currently subscribed to Gnus groups.")
            (defun ordenada-gnus--get-topic-groups ()
              "Return a flattened list of groups from `ordenada-gnus-topic-alist'."
              (flatten-list (mapcar (lambda (topic)
                                      (cdr topic))
                                    ordenada-gnus-topic-alist)))

            (defun ordenada-gnus-get-article-participants ()
              "Retrieve the participants from the current article."
              (if (and (gnus-alive-p)
                       (message-fetch-field "from")
                       (message-fetch-field "to"))
                  (with-current-buffer gnus-article-buffer
                    (string-join
                     (remove-if
                      (lambda (address)
                        (string-match user-mail-address address))
                      (append
                       (split-string (message-fetch-field "from") ", ")
                       (split-string (message-fetch-field "to") ", ")))
                     ", "))
                ""))

            (defun ordenada-gnus-shr-browse-url-new-window ()
              "When using shr, open links in a new window."
              (interactive)
              (shr-browse-url nil nil t))

            (define-minor-mode ordenada-gnus-topic-mode
              "Apply Gnus topic settings declaratively and subscribe to groups."
              :group 'ordenada-gnus
              (setq gnus-topic-topology ordenada-gnus-topic-topology)
              (setq gnus-topic-alist ordenada-gnus-topic-alist)
              (unless ordenada-gnus-subscribed-p
                (mapc (lambda (topic)
                        (gnus-subscribe-hierarchically topic))
                      (ordenada-gnus--get-topic-groups)))
              (setq ordenada-gnus-subscribed-p t))

            (setopt ordenada-gnus-topic-alist '(${
              toString (
                lib.mapAttrsToList (name: value: ''
                  ("${name}" ${
                    if value != [ ] then
                      toString (
                        map (x: ''
                          "${x}"
                        '') value
                      )
                    else
                      ""
                  })
                '') topicGroups
              )
            }))
            (setopt ordenada-gnus-topic-topology '(${toString topicTopology}))
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${key}" 'gnus))
            (setopt mail-user-agent 'gnus-user-agent)
            ${mkIf (hasFeature "emacs.dired" user) ''
              (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)
            ''}
            (with-eval-after-load 'gnus
              (setopt gnus-use-full-window nil)
              (setopt gnus-use-cache t)
              ${mkIf user.features.emacs.advancedUser ''
                (setq gnus-novice-user nil)
              ''}
              (setopt gnus-interactive-exit nil)
              (setopt gnus-thread-sort-functions
                      '(gnus-thread-sort-by-most-recent-date
                        (not gnus-thread-sort-by-number)))
              (setopt gnus-permanently-visible-groups "^nnmaildir")
              (setopt gnus-parameters '(${
                toString (
                  lib.mapAttrsToList (name: value: ''
                    ("${name}" ${
                      if value != { } then
                        toString (
                          lib.mapAttrsToList (name': value': ''
                            (${name'} . ${toString value'})
                          '') value
                        )
                      else
                        ""
                    })
                  '') groupParameters
                )
              }))
              (setopt gnus-directory "${directory}")
              (setopt gnus-home-directory (locate-user-emacs-file "gnus"))
              (setopt gnus-cache-directory "${directory}/news/cache")
              (setopt gnus-kill-files-directory "${directory}/news")
              (setopt gnus-article-save-directory "${directory}/news")
              (setopt gnus-large-newsgroup 100)
              ${mkIf (messageArchiveMethod != null) ''
                (setopt gnus-message-archive-method
                        '(${toString messageArchiveMethod}))
              ''}
              ${mkIf (messageArchiveGroup != null) ''
                (setopt gnus-message-archive-group
                        '(${toString messageArchiveGroup}))
              ''}
              (setopt gnus-update-message-archive-method t)
              (setopt gnus-posting-styles '(${
                toString (
                  lib.mapAttrsToList (name: acc: ''
                    ("${name}"
                     (address "${acc.fqda}")
                     ("Gcc" "nnmaildir+${name}:Sent"))
                    ${
                      if hasFeature "mail.msmtp" user then
                        with accounts.email.accounts.${name}.smtp;
                        ''
                          ("X-Message-SMTP-Method"
                           "smtp ${host} ${toString port} ${acc.fqda}")
                        ''
                      else
                        ""
                    }
                  '') user.features.mail.accounts
                )
              } ${toString postingStyles}))
              (setopt gnus-select-method '(nnnil))
              (setopt gnus-secondary-select-methods '(${
                mkIf (hasFeature "mail.mbsync" user) (
                  toString (
                    lib.mapAttrsToList (
                      name: acc: with accounts.email; ''
                        (nnmaildir "${name}"
                         (directory
                          "${maildirBasePath}/${accounts.${name}.maildir.path}"))
                      ''
                    ) user.features.mail.accounts
                  )
                )
              }
              (nntp "gwene" (nntp-address "news.gwene.org"))
              (nnfolder "archive"
                        (nnfolder-directory
                         "${accounts.email.maildirBasePath}/archive")
                        (nnfolder-active-file
                         "${accounts.email.maildirBasePath}/archive/active")
                        (nnfolder-get-new-mail nil)
                        (nnfolder-inhibit-expiry t)))))

            (with-eval-after-load 'mail-source
              (setopt mail-source-directory "${directory}/mail")
              (setopt mail-default-directory "${directory}"))

            (with-eval-after-load 'gnus-start
              (setopt gnus-dribble-directory "${directory}")
              (setopt gnus-startup-file "${directory}/.newsrc")
              (setopt gnus-subscribe-newsgroup-method
                      #'gnus-subscribe-hierarchically)
              (setopt gnus-check-new-newsgroups nil)
              (setopt gnus-save-killed-list nil))

            (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
            (add-hook 'gnus-group-mode-hook #'hl-line-mode)

            (with-eval-after-load 'gnus-sum
              (setopt gnus-summary-goto-unread 'never)
              (setopt gnus-thread-hide-subtree t))

            (with-eval-after-load 'nndraft
              (setopt nndraft-directory "${directory}/mail/drafts"))

            (add-hook 'gnus-topic-mode-hook #'ordenada-gnus-topic-mode)
            (with-eval-after-load 'gnus-topic
              (setopt gnus-gcc-mark-as-read t)
              (setq gnus-server-alist
                    '(("archive" nnfolder "archive"
                       (nnfolder-directory "${directory}/mail/archive")
                       (nnfolder-get-new-mail nil)
                       (nnfolder-inhibit-expiry t)))))

            (with-eval-after-load 'gnus-art
              (let ((map gnus-article-mode-map))
                (define-key map [remap shr-mouse-browse-url]
                            #'shr-mouse-browse-url-new-window)
                (define-key map [remap shr-browse-url]
                            #'ordenada-gnus-shr-browse-url-new-window))
              (setopt gnus-visible-headers
                      '("^From:" "^To:" "^Cc:" "^Subject:" "^Newsgroups:"
                        "^Date:" "Followup-To:" "Reply-To:" "^Organization:"
                        "^X-Newsreader:" "^X-Mailer:" "^Message-ID:"
                        "^In-Reply-To:" "^References:"))
              (setopt gnus-sorted-header-list gnus-visible-headers))
          '';
      };
    });
  };
}
