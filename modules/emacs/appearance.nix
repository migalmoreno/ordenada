{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  nurNoPkgs = import pkgs.inputs.nur {
    pkgs = null;
    nurpkgs = pkgs;
  };
  inherit (lib) mkEnableOption mkOption types;
in
{
  options = {
    ordenada.features.emacs.appearance = {
      enable = mkEnableOption "the Emacs appearance feature";
      fringes = mkOption {
        type = types.int;
        description = "The width of the window's frame fringes.";
        default = 8;
      };
      margin = mkOption {
        type = types.int;
        description = "The margin in between Emacs windows.";
        default = 8;
      };
      modeLinePadding = mkOption {
        type = types.int;
        description = "The padding for the Emacs mode line.";
        default = 4;
      };
      headerLinePadding = mkOption {
        type = types.int;
        description = "The padding for the Emacs header line.";
        default = 4;
      };
      tabBarPadding = mkOption {
        type = types.int;
        description = "The padding for the Emacs tab bar.";
        default = 4;
      };
      headerLineAsModeLine = mkEnableTrueOption "the Emacs header line to be used as the mode line";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.appearance" (
      user: with user.features.emacs.appearance; {
        imports = [ nurNoPkgs.repos.rycee.hmModules.emacs-init ];
        programs.emacs = mkElispConfig {
          name = "ordenada-appearance";
          config = ''
            (require 'xdg)
            (setq minibuffer-message-timeout 0)
            (pixel-scroll-precision-mode 1)
            (tooltip-mode 0)
            (setopt mode-line-compact 'long)
            (fset 'yes-or-no-p 'y-or-n-p)
            (blink-cursor-mode 0)
            (set-default 'cursor-type '(bar . 1))
            (setq-default cursor-in-non-selected-windows nil)
            (setopt bookmark-set-fringe-mark nil)
            (with-eval-after-load 'menu-bar
              (menu-bar-mode 0))
            (with-eval-after-load 'tool-bar
              (tool-bar-mode 0))
            (with-eval-after-load 'scroll-bar
              (scroll-bar-mode 0))
            (with-eval-after-load 'fringe
              (fringe-mode ${toString fringes}))
            (set-frame-parameter nil 'internal-border-width ${toString margin})
            (setopt use-dialog-box nil)
            (setopt use-file-dialog nil)
            (setopt window-divider-default-right-width ${toString margin})
            (setopt window-divider-default-bottom-width ${toString margin})
            (window-divider-mode)
            (setopt frame-inhibit-implied-resize t)
            (setq frame-title-format '(multiple-frames "%b" ("" "%b")))
            (with-eval-after-load 'minions-autoloads
              (minions-mode))
            (with-eval-after-load 'minions
              (setopt minions-mode-line-lighter ";"))
            ${mkIf headerLineAsModeLine ''
              (setq minions-mode-line-minor-modes-map
                    (let ((map (make-sparse-keymap)))
                      (define-key map [header-line down-mouse-1]
                                  #'minions-minor-modes-menu)
                      map))
              (defun ordenada-appearance--move-mode-line-to-header ()
                "Move mode-line to header-line.
              This function is needed for various modes to set up the mode-line late."
                (setq-local header-line-format mode-line-format)
                (setq-local mode-line-format nil))

              (add-hook 'calendar-initial-window-hook
                        #'ordenada-appearance--move-mode-line-to-header)
              (setq-default header-line-format mode-line-format)
              (setq-default mode-line-format nil)
            ''}'';
          elispPackages = with pkgs.emacsPackages; [ minions ];
          earlyInit = ''
            (push '(menu-bar-lines . 0) default-frame-alist)
            (push '(tool-bar-lines . 0) default-frame-alist)
            (push '(vertical-scroll-bars) default-frame-alist)
            (push '(horizontal-scroll-bars) default-frame-alist)
            (push (cons 'left-fringe ${toString fringes}) default-frame-alist)
            (push (cons 'right-fringe ${toString fringes}) default-frame-alist)
            (push '(no-special-glyphs) default-frame-alist)
            (push '(undecorated) default-frame-alist)
            (setq menu-bar-mode nil
                  tool-bar-mode nil
                  scroll-bar-mode nil)
            (push '(internal-border-width . ${toString margin}) default-frame-alist)
            (setq inhibit-startup-screen t)
            (setq initial-scratch-message nil)
          '';
        };
      }
    );
  };
}
