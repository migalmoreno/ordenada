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
        description = "Primary Git username";
        default = "";
      };
      email = mkOption {
        type = types.str;
        description = "Primary Git email";
        default = "";
      };
      signCommits = mkEnableOption "GPG signing of commits";
      signingKey = mkOption {
        type = types.str;
        description = "The GPG key fingerprint to use to sign commits.";
        default = "";
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
          (add-hook 'magit-mode-hook 'toggle-truncate-lines)
          (with-eval-after-load 'project
            (define-key project-prefix-map "m" 'magit-project-status)
            (add-to-list 'project-switch-commands
                         '(magit-project-status "Show Magit Status")))
          (with-eval-after-load 'magit
            (define-key magit-mode-map "q" #'magit-kill-this-buffer)
            (setq magit-display-buffer-function
                  #'magit-display-buffer-same-window-except-diff-v1)
            (setq magit-pull-or-fetch t)
            (require 'forge))

          (with-eval-after-load 'ordenada-keymaps
            (define-key ordenada-app-map (kbd "g l") #'git-link))
          (with-eval-after-load 'git-link
            ${
              toString (
                lib.attrsets.mapAttrsToList (url: fn: ''
                  (add-to-list 'git-link-remote-alist '("${url}" ${fn}))
                  (add-to-list 'git-link-commit-remote-alist '("${url}" ${fn}))
                '') user.features.git.gitLinkRemotes
              )
            })
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
