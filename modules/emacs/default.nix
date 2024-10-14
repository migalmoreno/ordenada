{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib)
    mkEnableOption
    mkPackageOption
    mkOption
    types
    ;
in
{
  imports = [
    ./ace-window.nix
    ./all-the-icons.nix
    ./appearance.nix
    ./completion.nix
    ./consult.nix
    ./corfu.nix
    ./daemons.nix
    ./dired.nix
    ./ebdb.nix
    ./embark.nix
    ./eshell.nix
    ./help.nix
    ./keymaps.nix
    ./marginalia.nix
    ./modus-themes.nix
    ./orderless.nix
    ./org.nix
    ./org-roam.nix
    ./programming.nix
    ./project.nix
    ./shell.nix
    ./smartparens.nix
    ./spelling.nix
    ./vertico.nix
    ./vterm.nix
    ./tramp.nix
  ];
  options = {
    ordenada.features.emacs = {
      enable = mkEnableOption "the Emacs feature";
      package = mkPackageOption pkgs "emacs" { default = "emacs29-pgtk"; };
      advancedUser = mkEnableOption "advanced user mode for Emacs features.";
      defaultThemes = mkOption {
        type = types.attrs;
        description = "The light theme to use for Emacs.";
      };
      extraPackages = mkOption {
        type = types.listOf types.package;
        description = "List of extra Emacs packages.";
        default = [ ];
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs" (
      user:
      lib.mkMerge [
        (lib.mkIf user.features.emacs.enable {
          services.emacs = {
            enable = true;
            defaultEditor = true;
            client.enable = true;
            socketActivation.enable = true;
            startWithUserSession = true;
          };
          programs.emacs = {
            enable = true;
            package = user.features.emacs.package;
            extraPackages = epkgs: user.features.emacs.extraPackages;
          };
        })
        {
          programs.emacs = mkElispConfig {
            name = "ordenada-base";
            config = ''
              (defgroup ordenada nil
                "Base customization group for ordenada settings."
                :group 'external
                :prefix 'ordenada-)
              (setq gc-cons-threshold most-positive-fixnum
                    gc-cons-percentage 0.6)
              (add-hook 'emacs-startup-hook
                        (lambda ()
                          (setq undo-limit (* 8 1024 1024)
                                read-process-output-max (* 1024 1024))))
              (advice-add 'x-apply-session-resources :override 'ignore)
              (setq native-comp-jit-compilation nil)
              (setq custom-file
                   (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                           "/emacs/custom.el"))
              (load custom-file t)
              (setq backup-directory-alist
                    `(,(cons "." (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                         "/emacs/backup"))))
              (setq bookmark-default-file
                    (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                            "/emacs/bookmarks"))
              (setq auto-save-list-file-prefix
                    (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                            "/emacs/auto-save-list"))
              (save-place-mode 1)
              (setq save-place-file
                    (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                            "/emacs/places"))
              (setq history-length 10000)
              (setq savehist-file
                    (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                            "/emacs/history"))
              (show-paren-mode 1)
              (subword-mode 1)
              (setq-default indent-tabs-mode nil)
              (setq save-interprogram-paste-before-kill t)
              (setq mouse-yank-at-point t)
              (setq require-final-newline t)
              (repeat-mode 1)
              (setq copyright-names-regexp
                    (format "%s <%s>" user-full-name user-mail-address))
              (add-hook 'after-save-hook 'copyright-update)
              (add-hook 'before-save-hook 'delete-trailing-whitespace)

              (define-key global-map (kbd "M-K") 'kill-whole-line)
              (define-key global-map (kbd "M-c") 'capitalize-dwim)
              (define-key global-map (kbd "M-l") 'downcase-dwim)
              (define-key global-map (kbd "M-u") 'upcase-dwim)

              (with-eval-after-load 'mwheel
                (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                                    ((control) . 1)))
                (setq mouse-wheel-progressive-speed nil)
                (setq mouse-wheel-follow-mouse t)
                (setq scroll-conservatively 100)
                (setq mouse-autoselect-window nil)
                (setq what-cursor-show-names t)
                (setq focus-follows-mouse t))
            '';
          };
        }
      ]
    );
  };
}
