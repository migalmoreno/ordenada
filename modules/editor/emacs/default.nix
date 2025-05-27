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
    ./calendar.nix
    ./completion.nix
    ./consult.nix
    ./corfu.nix
    ./daemons.nix
    ./dired.nix
    ./ebdb.nix
    ./embark.nix
    ./eshell.nix
    ./help.nix
    ./gnus.nix
    ./keymaps.nix
    ./marginalia.nix
    ./message.nix
    ./modus-themes.nix
    ./orderless.nix
    ./org.nix
    ./org-roam.nix
    ./pdf-tools.nix
    ./programming.nix
    ./project.nix
    ./shell.nix
    ./smartparens.nix
    ./spelling.nix
    ./vertico.nix
    ./vterm.nix
    ./time.nix
    ./tramp.nix
    ./which-key.nix
  ];
  options = {
    ordenada.features.emacs = {
      enable = mkEnableOption "the Emacs feature";
      package = mkPackageOption pkgs "emacs" { default = "emacs30-pgtk"; };
      advancedUser = mkEnableOption "advanced user mode for Emacs features";
      defaultThemes = mkOption {
        type = types.attrs;
        description = "The light theme to use for Emacs.";
        default = { };
      };
      extraConfig = mkOption {
        type = types.lines;
        description = "Extra Emacs configuration.";
        default = '''';
      };
      extraPackages = mkOption {
        type = types.listOf types.package;
        description = "List of extra Emacs packages.";
        default = [ ];
      };
      autoUpdateBuffers = mkEnableTrueOption "automatically updating buffers";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs" (
      user:
      with user.features.emacs;
      lib.mkMerge [
        (lib.mkIf enable {
          services.emacs = {
            enable = true;
            defaultEditor = true;
            client.enable = true;
            socketActivation.enable = true;
            startWithUserSession = true;
          };
          programs.emacs = {
            enable = true;
            package = package;
            extraConfig = extraConfig;
            extraPackages = epkgs: extraPackages;
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
              (setq gc-cons-threshold most-positive-fixnum)
              (setq gc-cons-percentage 0.6)
              (add-hook 'emacs-startup-hook
                        (lambda ()
                          (setq undo-limit (* 8 1024 1024)
                                read-process-output-max (* 1024 1024))))
              (advice-add 'x-apply-session-resources :override 'ignore)
              (setq native-comp-jit-compilation nil)
              (setopt user-full-name "${user.features.userInfo.fullName}")
              (setopt user-mail-address "${user.features.userInfo.email}")
              (setopt custom-file
                      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                              "/emacs/custom.el"))
              (load custom-file t)
              (setopt backup-directory-alist
                      `(,(cons "." (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                           "/emacs/backup"))))
              (setopt bookmark-default-file
                      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                              "/emacs/bookmarks"))
              (setopt auto-save-list-file-prefix
                      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                              "/emacs/auto-save-list"))
              (save-place-mode 1)
              (setopt save-place-file
                      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                              "/emacs/places"))
              (setopt recentf-save-file
                      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                  "/emacs/recentf"))
              (recentf-mode 1)
              (run-with-idle-timer 30 t #'recentf-save-list)
              (setopt history-length 10000)
              (setopt savehist-file
                      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                              "/emacs/history"))
              (add-hook 'after-init-hook #'savehist-mode)
              (run-with-idle-timer 30 t #'savehist-save)

              (show-paren-mode 1)
              (subword-mode 1)
              (setq-default indent-tabs-mode nil)
              (setopt save-interprogram-paste-before-kill t)
              (setopt mouse-yank-at-point t)
              (setopt require-final-newline t)
              (repeat-mode 1)
              (setopt copyright-names-regexp
                      (format "%s <%s>" user-full-name user-mail-address))
              (add-hook 'after-save-hook 'copyright-update)
              (add-hook 'before-save-hook 'delete-trailing-whitespace)

              (keymap-global-set "M-K" #'kill-whole-line)
              (keymap-global-set "M-c" #'capitalize-dwim)
              (keymap-global-set "M-l" #'downcase-dwim)
              (keymap-global-set "M-u" #'upcase-dwim)

              ${mkIf autoUpdateBuffers ''
                (setopt global-auto-revert-non-file-buffers t)
                (global-auto-revert-mode 1)
              ''}

              (with-eval-after-load 'mwheel
                (setopt mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                                       ((control) . 1)))
                (setopt mouse-wheel-progressive-speed nil)
                (setopt mouse-wheel-follow-mouse t)
                (setopt scroll-conservatively 100)
                (setopt mouse-autoselect-window nil)
                (setopt what-cursor-show-names t)
                (setopt focus-follows-mouse t))
            '';
          };
        }
      ]
    );
  };
}
