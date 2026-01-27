{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "git";
  options =
    { config, ... }:
    let
      inherit (lib) types mkOption mkEnableOption;
    in
    {
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
        default = config.ordenada.features.userInfo.gpgPrimaryKey;
      };
      gitLinkRemotes = mkOption {
        type = types.attrs;
        description = "Attrs of Git remote URLs to git-link functions.";
        default = { };
      };
      extraSettings = mkOption {
        type = types.attrs;
        description = "Extra Git configuration settings.";
        default = { };
      };
    };
  homeManager =
    { config, pkgs, ... }:
    let
      inherit (config.ordenada.features) git emacs;
    in
    {
      programs.gh = {
        enable = true;
      };
      programs.git = with config.ordenada.features.git; {
        enable = true;
        settings = {
          user = {
            inherit email;
            name = username;
          };
        }
        // extraSettings;
        signing = {
          signByDefault = signCommits;
          key = signingKey;
        };
      };
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-git";
        config = # elisp
          ''
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

            ${lib.optionalString emacs.consult.enable ''
              (with-eval-after-load 'consult
                (require 'consult-gh-transient)
                (require 'consult-gh-embark)
                (require 'consult-gh-forge)
                (consult-gh-embark-mode 1)
                (consult-gh-forge-mode 1))
            ''}

            (with-eval-after-load 'vc
              (setopt vc-follow-symlinks t))

            (with-eval-after-load 'ediff
              (setopt ediff-window-setup-function #'ediff-setup-windows-plain))

            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "L" #'git-link))
            (with-eval-after-load 'git-link
              ${
                toString (
                  lib.mapAttrsToList (url: fn: ''
                    (add-to-list 'git-link-remote-alist '("${url}" ${fn}))
                    (add-to-list 'git-link-commit-remote-alist '("${url}" ${fn}))
                  '') git.gitLinkRemotes
                )
              })
            (with-eval-after-load 'vc-mode
              (setcdr (assq 'vc-mode mode-line-format)
                    '((:eval (truncate-string-to-width vc-mode 25 nil nil "...")))))
          '';
        elispPackages = with pkgs.emacsPackages; [
          magit
          forge
          git-link
          pr-review
          consult-gh
          consult-gh-embark
          consult-gh-forge
          consult-gh-with-pr-review
        ];
      };
    };
}
