{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "slack";
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
      accounts = mkOption {
        type = types.attrsOf (
          types.submodule (
            { name, ... }:
            {
              options = {
                nick = mkOption {
                  type = types.str;
                  default = name;
                  description = "Slack nick registered under the associated workspace.";
                };
                workspace = mkOption {
                  type = types.str;
                  default = "";
                  example = "clojurians";
                  description = "The Slack workspace to authenticate with.";
                };
                useCookie = mkEnableOption ''
                  Whether to use a browser cookie for authentication. As per
                  https://github.com/yuya373/emacs-slack#how-to-get-token-and-cookie
                  you only need to set this to true if your Slack token begins
                  with "xoxc-".
                '';
              };
            }
          )
        );
        default = { };
        description = "Attrset of IRC accounts.";
      };
      emacs-slack = {
        package = mkPackageOption pkgs [ "emacsPackages" "slack" ] { };
        key = mkOption {
          type = types.str;
          description = "The prefix key used to for slack.el operations.";
          default = "s";
        };
      };
    };
  homeManager =
    { config, pkgs, ... }:
    let
      inherit (config.ordenada.features) emacs slack;
    in
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-slack";
        config = # elisp
          ''
            (eval-when-compile
              (require 'cl-macs))
            (defgroup ordenada-slack nil
              "Utilities for slack.el, the Emacs Slack client."
              :group 'ordenada)
            (cl-defstruct ordenada-slack-team workspace nick cookie-p)
            (defcustom ordenada-slack-teams '()
              "List of `ordenada-slack-team' structs that hold Slack accounts."
              :type '(repeat ordenada-slack-team)
              :group 'ordenada-slack)
            (defvar ordenada-slack-map nil
              "Map to bind `slack' commands under.")
            (define-prefix-command 'ordenada-slack-map)

            ${lib.optionalString emacs.consult.initialNarrowing ''
                            (defvar ordenada-slack-buffer-source
                              `(:name "Slack"
                                      :narrow ?s
                                      :category buffer
                                      :state ,'consult--buffer-state
                                      :items ,(lambda ()
                                                (mapcar #'buffer-name
                                                        (ordenada-completion--mode-buffers
                                                         'slack-message-buffer-mode
                                                         'slack-thread-message-buffer-mode))))
                              "Source for Slack buffers to be set in
              `consult-buffer-sources'.")
                          (with-eval-after-load 'consult
                            (add-to-list 'consult-buffer-sources
                                         ordenada-slack-buffer-source 'append))
                          (with-eval-after-load 'ordenada-completion
                            (add-to-list 'ordenada-completion-initial-narrow-alist
                                         '(slack-message-buffer-mode . ?s))
                            (add-to-list 'ordenada-completion-initial-narrow-alist
                                         '(slack-thread-message-buffer-mode . ?s)))
            ''}

            (defun ordenada-slack-connect (team)
              "Connect to Slack TEAM with personal credentials."
              (interactive
               (list (cl-find (completing-read
                               "Team: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     `(metadata
                                       ,(cons 'display-sort-function 'identity))
                                   (complete-with-action
                                    action
                                    (mapcar #'ordenada-slack-team-workspace
                                            ordenada-slack-teams)
                                    string pred))))
                              ordenada-slack-teams
                              :key #'ordenada-slack-team-workspace
                              :test 'string=)))
              (let ((workspace (ordenada-slack-team-workspace team)))
                (slack-register-team
                 :name workspace
                 :token (auth-source-pick-first-password
                         :host workspace
                         :user (ordenada-slack-team-nick team))
                 :cookie (when (ordenada-slack-team-cookie-p team)
                           (auth-source-pick-first-password
                            :host workspace
                            :user (concat (ordenada-slack-team-nick team)
                                          "^cookie"))))
                (slack-change-current-team)))

            (setq ordenada-slack-teams
                  (list ${
                    toString (
                      lib.mapAttrsToList (name: acc: ''
                        (make-ordenada-slack-team
                         :workspace "${acc.workspace}"
                         :nick "${acc.nick}"
                         :cookie-p ${ordenada-lib.elisp.toBoolean acc.useCookie}
                         )
                      '') slack.accounts
                    )
                  }))

            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${slack.emacs-slack.key}"
                          #'ordenada-slack-map)
              (keymap-set ordenada-slack-map "c" #'ordenada-slack-connect))
            (with-eval-after-load 'slack
              (setq slack-buffer-emojify t)
              (setq slack-prefer-current-team t)
              (setq slack-buffer-function #'switch-to-buffer)
              (let ((map ordenada-slack-map))
                (keymap-set map "s" #'slack-channel-select)
                (keymap-set map "t" #'slack-change-current-team))
              (set-face-attribute 'slack-preview-face nil
                                  :background 'unspecified))
          '';
        elispPackages = [ slack.emacs-slack.package ];
      };
    };
}
