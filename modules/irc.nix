{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.emacs;
  inherit (lib) mkEnableOption mkOption types;
  ercBufferBehavior = types.nullOr (
    types.enum [
      "window"
      "window-no-select"
      "frame"
      "bury"
      "buffer"
    ]
  );
  accountOpts =
    { name, config, ... }:
    {
      options = {
        nick = mkOption {
          type = types.str;
          default = name;
          description = "Nick the account is registered under the IRC network.";
        };
        network = mkOption {
          type = types.str;
          default = "irc.libera.chat";
          description = "IRC network to use for this account.";
        };
        port = mkOption {
          type = types.int;
          default = cfg.erc.defaultPort;
          description = "Port number to use for the IRC connection on this account.";
        };
        client = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Client name used for the IRC connection on this account.";
        };
        tls = mkEnableTrueOption "using TLS for the IRC network connection";
        bouncer = mkEnableOption "considering this account as connected to an IRC bouncer server";
      };
    };
in
{
  options = {
    ordenada.features = {
      irc = {
        enable = mkEnableOption "the IRC feature";
        accounts = mkOption {
          type = types.attrsOf (types.submodule accountOpts);
          default = { };
          description = "Attrset of IRC accounts.";
        };
      };
      emacs.erc = {
        defaultServer = mkOption {
          type = types.str;
          default = "irc.libera.chat";
          description = "Default IRC server name to use in ERC.";
        };
        defaultPort = mkOption {
          type = types.int;
          default = 6697;
          description = "Default IRC server port to use in ERC.";
        };
        fullName = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Full name to use for ERC users.";
        };
        nick = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Nick to use for ERC users.";
        };
        hideList = mkOption {
          type = types.listOf types.str;
          default = [
            "NICK"
            "JOIN"
            "PART"
            "QUIT"
            "MODE"
            "AWAY"
          ];
          description = "List of message types to hide.";
        };
        killBuffersOnQuit = mkEnableTrueOption "killing ERC buffers on quit";
        headerLineFormat = mkOption {
          type = types.nullOr types.str;
          default = " %n on %t (%m,%l)";
          description = "Format string to be shown as the header-line in ERC buffers.";
        };
        autoQuery = mkOption {
          type = ercBufferBehavior;
          default = "window-no-select";
          description = "Buffer behavior upon receiving a private message.";
        };
        joinBuffer = mkOption {
          type = ercBufferBehavior;
          default = "bury";
          description = "Buffer behavior upon joining a newly created IRC buffer.";
        };
        queryDisplay = mkOption {
          type = ercBufferBehavior;
          default = "window";
          description = "Buffer behavior when using the /QUERY command to talk to someone.";
        };
        alignNicknames = mkEnableTrueOption "aligning nicks in ERC buffers";
        alignNicknamesColumn = mkOption {
          type = types.int;
          default = 20;
          description = "Column around which nicknames will be aligned.";
        };
        showImages = mkEnableOption "showing images in ERC buffers";
        log = mkEnableOption "logging ERC sessions";
        trackExcludeTypes = mkOption {
          type = types.listOf types.str;
          default = [
            "324"
            "329"
            "JOIN"
            "MODE"
            "NICK"
            "PART"
            "QUIT"
          ];
          description = "List of message types to ignore from notifications.";
        };
        autojoin = mkEnableTrueOption "joining channels automatically";
        autojoinChannels = mkOption {
          type = types.attrsOf (types.listOf types.str);
          default = { };
          description = "Attrset of channels to autojoin in given IRC networks.";
        };
        sidebarHeaderLineFormat = mkOption {
          type = types.nullOr types.str;
          default = if cfg.appearance.headerLineAsModeLine then " ERC Status" else null;
          description = "The header line format for the ERC status side bar.";
        };
        sidebarModeLineFormat = mkOption {
          type = types.nullOr types.str;
          default = if cfg.appearance.headerLineAsModeLine then null else "ERC Status";
          description = "The header line format for the ERC status side bar.";
        };
        sidebarWidth = mkOption {
          type = types.int;
          default = 30;
          description = "The width of the ERC status sidebar.";
        };
        promptForPassword = mkEnableOption "prompting for password upon connecting to an IRC network";
        key = mkOption {
          type = types.str;
          description = "Keybinding to launch ERC.";
          default = "i";
        };
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "irc" (user: {
      programs.emacs =
        with user.features.emacs;
        with erc;
        mkElispConfig {
          name = "ordenada-erc";
          config = ''
            (eval-when-compile
              (require 'erc)
              (require 'xdg)
              (require 'cl-macs))
            (defgroup ordenada-erc nil
              "Extra customizations for ERC."
              :group 'ordenada)
            (cl-defstruct ordenada-erc-user id network port nick tls-p bouncer-p client)
            (defcustom ordenada-erc-users '()
              "A list of `ordenada-erc-user' structs that hold IRC accounts."
              :type '(repeat ordenada-erc-user)
              :group 'ordenada-erc)
            ${mkIf ((hasFeature "emacs.consult" user) && consult.initialNarrowing) ''
              (autoload 'erc-buffer-list "erc")
              (defvar ordenada-erc-buffer-source
                `(:name "ERC"
                  :narrow ?i
                  :category buffer
                  :state ,'consult--buffer-state
                  :items ,(lambda () (mapcar 'buffer-name (erc-buffer-list))))
                "Source for ERC buffers to be set in
                `consult-buffer-sources'.")
              (with-eval-after-load 'consult
                (add-to-list 'consult-buffer-sources ordenada-erc-buffer-source
                             'append))
              (with-eval-after-load 'ordenada-completion
                  (add-to-list 'ordenada-completion-initial-narrow-alist
                               '(erc-mode . ?i)))
            ''}
            (defun ordenada-erc-connect (user)
              "Connect USER to irc network via tls."
              (interactive
               (list (cl-find (intern (completing-read
                                       "User: "
                                       (lambda (string pred action)
                                         (if (eq action 'metadata)
                                             `(metadata
                                               ,(cons 'display-sort-function 'identity))
                                             (complete-with-action
                                              action
                                              (mapcar #'ordenada-erc-user-id ordenada-erc-users)
                                              string pred)))))
                              ordenada-erc-users :key 'ordenada-erc-user-id)))
              (require 'erc)
              (let* (username
                     (network (ordenada-erc-user-network user))
                     (nick (ordenada-erc-user-nick user))
                     (password (auth-source-pick-first-password
                                :host network
                                :user nick)))
                (when (ordenada-erc-user-bouncer-p user)
                  (let* ((irc-network (completing-read
                                       "Network: "
                                       (lambda (string pred action)
                                         (if (eq action 'metadata)
                                             `(metadata
                                               ,(cons 'display-sort-function 'identity))
                                           (complete-with-action
                                            action
                                            (mapcar #'ordenada-erc-user-network
                                                    (cl-remove network ordenada-erc-users
                                                               :key 'ordenada-erc-user-network
                                                               :test 'string=))
                                            string pred)))))
                         (irc-network-nick (ordenada-erc-user-nick
                                            (cl-find irc-network ordenada-erc-users
                                                     :key 'ordenada-erc-user-network
                                                     :test 'string=))))
                    (setq username (concat (format "%s/%s" irc-network-nick irc-network)
                                           (when (ordenada-erc-user-client user)
                                             (concat "@" (ordenada-erc-user-client user)))))))
                (funcall (if (ordenada-erc-user-tls-p user) #'erc-tls #'erc)
                         :server network
                         :port (ordenada-erc-user-port user)
                         :nick nick
                         :user username
                         :password password)))

            (defun ordenada-erc-close-buffers ()
              "Close all erc buffers upon closing the erc server process."
              (interactive)
              (mapc 'kill-buffer (erc-buffer-list nil erc-server-process)))

            (defun ordenada-erc-toggle-timestamps ()
              "Refresh and toggle the timestamps in the current erc buffer."
              (interactive)
              (erc-toggle-timestamps)
              (force-window-update (selected-window)))

            (defun ordenada-erc-window-reuse-condition (buf-name action)
              "Set up a condition for erc buffers to be reused."
              (with-current-buffer buf-name
                (when (eq major-mode 'erc-mode)
                  (not action))))

            (defun ordenada-erc-status-sidebar-toggle ()
              "Toggle the status sidebar by killing its buffer when closed."
              (interactive)
              (if (get-buffer-window erc-status-sidebar-buffer-name nil)
                  (progn
                    (erc-status-sidebar-close)
                    (kill-buffer erc-status-sidebar-buffer-name))
                (erc-status-sidebar-open)))

            (defun ordenada-erc-status-add-padding (fun channame &optional num-messages erc-face)
              "Add left padding on the sidebar formatted channels list."
              (concat " " (funcall fun channame num-messages erc-face)))

            (defun erc-cmd-LATEST (&optional channel)
              "Manually retrieve latest messages from an IRCv3 bouncer."
              (interactive "sTarget Channel: ")
              (let ((channel (if (> (length channel) 0)
                                 channel
                               (erc-default-target))))
                (erc-cmd-QUOTE
                 (format "CHATHISTORY LATEST %s * %s" channel 1000))))

            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${key}" #'ordenada-erc-connect))

            (setq ordenada-erc-users
                  (list ${
                    toString (
                      lib.mapAttrsToList (name: acc: ''
                        (make-ordenada-erc-user
                         :id '${name}
                         :network "${acc.network}"
                         :port ${toString acc.port}
                         :nick "${acc.nick}"
                         :tls-p ${mkBoolean acc.tls}
                         :client ${mkNilOr acc.client ''"${acc.client}"''}
                         :bouncer-p ${mkBoolean acc.bouncer})
                      '') user.features.irc.accounts
                    )
                  }))

            (add-to-list 'display-buffer-alist
                         (cons 'ordenada-erc-window-reuse-condition
                               '(display-buffer-reuse-mode-window
                                 (inhibit-same-window . t)
                                 (inhibit-switch-frame . t)
                                 (mode . erc-mode))))

            (with-eval-after-load 'erc-status-sidebar
              (advice-add 'erc-status-sidebar-default-chan-format
                          :around #'ordenada-erc-status-add-padding)
              (setq erc-status-sidebar-mode-line-format
              ${mkNilOr sidebarModeLineFormat ''"${sidebarModeLineFormat}"''})
              (setq erc-status-sidebar-header-line-format
              ${mkNilOr sidebarHeaderLineFormat ''"${sidebarHeaderLineFormat}"''})
              (setopt erc-status-sidebar-width ${toString sidebarWidth}))

            (with-eval-after-load 'erc
              (require 'erc-status-sidebar)
              (require 'xdg)
              (let ((map erc-mode-map))
                (keymap-set map "C-c C-q" #'ordenada-erc-close-buffers)
                (keymap-set map "C-c C-t" #'ordenada-erc-toggle-timestamps)
                (keymap-set map "C-c C-s" #'ordenada-erc-status-sidebar-toggle))
              (setopt erc-default-server "${defaultServer}")
              (setopt erc-default-port ${toString defaultPort})
              ${mkIf (nick != null) ''(setopt erc-nick "${nick}")''}
              ${
                mkIf (fullName != null) ''
                  (setopt erc-user-full-name "${fullName}")
                ''
              }
              (setopt erc-hide-list ${mkList hideList})
              (setopt erc-hide-prompt t)
              (setopt erc-hide-timestamps t)
              (setopt erc-echo-timestamps nil)
              ${mkIf killBuffersOnQuit ''
                (setopt erc-kill-buffer-on-part t)
                (setopt erc-kill-server-buffer-on-quit t)
                (setopt erc-kill-queries-on-quit t)
              ''}
              (setopt erc-rename-buffers t)
              (setopt erc-header-line-format ${mkNilOr headerLineFormat ''"${headerLineFormat}"''})
              (setopt erc-auto-query ${mkNilOr autoQuery "'${autoQuery}"})
              (setopt erc-query-display ${mkNilOr queryDisplay "'${queryDisplay}"})
              (setopt erc-join-buffer ${mkNilOr joinBuffer "'${joinBuffer}"})
              (setopt erc-timestamp-format "%H:%M")
              (setopt erc-prompt-for-password ${mkBoolean promptForPassword})

              (add-to-list 'erc-modules 'keep-place)
              (add-to-list 'erc-modules 'notifications)
              (with-eval-after-load 'erc-track
                (setopt erc-track-exclude-server-buffer t)
                (setopt erc-track-enable-keybindings t)
                (setopt erc-track-shorten-start 8)
                (setopt erc-track-exclude-types ${mkList trackExcludeTypes}))
              ${
                if autojoin then
                  ''
                    (with-eval-after-load 'erc-join
                      (setopt erc-autojoin-timing 'connect)
                      (setopt erc-autojoin-delay 5)
                      (setopt erc-autojoin-channels-alist '(${
                        toString (
                          lib.mapAttrsToList (
                            name: chans: ''("${name}" ${toString (map (chan: ''"${chan}"'') chans)})''
                          ) autojoinChannels
                        )
                      })))
                  ''
                else
                  "(erc-autojoin-mode 0)"
              }
              (with-eval-after-load 'erc-backends
                (setopt erc-server-reconnect-timeout 3)
                (setopt erc-server-reconnect-attempts t))

              (add-to-list 'erc-modules 'services)
              (with-eval-after-load 'erc-services
                (setopt erc-prompt-for-nickserv-password nil))
              ${mkIf (hasFeature "emacs.spelling" user) ''
                (add-to-list 'erc-modules 'spelling)
              ''}
              ${mkIf log ''
                (add-to-list 'erc-modules 'log)
                (with-eval-after-load 'erc-log
                  (setopt erc-log-insert-log-on-open t)
                  (setopt erc-log-channels-directory
                          (expand-file-name "emacs/erc-logs"
                                            (xdg-cache-home))))
              ''}
              ${mkIf alignNicknames ''
                (with-eval-after-load 'erc-fill
                  (setopt erc-fill-function 'erc-fill-static)
                  (setopt erc-fill-static-center ${toString alignNicknamesColumn})
                  (setopt erc-fill-column 82))
              ''}
              ${mkIf showImages ''
                (add-to-list 'erc-modules 'image)
                (with-eval-after-load 'erc-image
                  (setopt erc-image-inline-rescale 100))
              ''})
          '';
          elispPackages = with pkgs.emacsPackages; [ erc-hl-nicks ] ++ lib.optional showImages erc-image;
        };
    });
  };
}
