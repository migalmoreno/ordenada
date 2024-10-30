{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) types mkOption mkEnableOption;
in
{
  options = {
    ordenada.features.git = {
      enable = mkEnableOption "the Git feature";
      username = mkOption {
        type = types.str;
        description = "Primary Git username.";
        default = config.ordenada.features.userInfo.fullName;
      };
      email = mkOption {
        type = types.str;
        description = "Primary Git email.";
        default = config.ordenada.features.userInfo.email;
      };
      signCommits = mkEnableOption "GPG signing of commits";
      signingKey = mkOption {
        type = types.nullOr types.str;
        description = "The GPG key fingerprint to use to sign commits.";
        default = null;
      };
      gitLinkRemotes = mkOption {
        type = types.attrs;
        description = "Attrs of Git remote URLs to git-link functions.";
        default = { };
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "git" (user: {
      programs.git = with user.features.git; {
        enable = true;
        userName = username;
        userEmail = email;
        signing = {
          signByDefault = signCommits;
          key = signingKey;
        };
      };
      programs.emacs = mkElispConfig {
        name = "ordenada-git";
        config = ''
          (add-hook 'magit-mode-hook #'toggle-truncate-lines)
          (with-eval-after-load 'project
            (keymap-set project-prefix-map "m" #'magit-project-status)
            (add-to-list 'project-switch-commands
                         '(magit-project-status "Show Magit Status")))
          (with-eval-after-load 'magit
            (keymap-set magit-mode-map "q" #'magit-kill-this-buffer)
            (setopt magit-display-buffer-function
                    #'magit-display-buffer-same-window-except-diff-v1)
            (setopt magit-pull-or-fetch t)
            (require 'forge))

          (with-eval-after-load 'vc
            (setopt vc-follow-symlinks t))

          (with-eval-after-load 'ediff
            (setopt ediff-window-setup-function #'ediff-setup-windows-plain))

          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "g l" #'git-link))
          (with-eval-after-load 'git-link
            ${
              toString (
                lib.attrsets.mapAttrsToList (url: fn: ''
                  (add-to-list 'git-link-remote-alist '("${url}" ${fn}))
                  (add-to-list 'git-link-commit-remote-alist '("${url}" ${fn}))
                '') user.features.git.gitLinkRemotes
              )
            })
          (setcdr (assq 'vc-mode mode-line-format)
                  '((:eval (truncate-string-to-width vc-mode 25 nil nil "..."))))
        '';
        elispPackages = with pkgs.emacsPackages; [
          magit
          forge
          git-link
        ];
      };
    });
  };
}
