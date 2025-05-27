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
    ordenada.features.password-store = {
      enable = mkEnableOption "the password-store feature";
      package = mkOption {
        type = types.package;
        default = pkgs.pass-wayland.withExtensions (exts: [ exts.pass-otp ]);
        description = "The package to use for password-store.";
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "password-store" (user: {
      programs.password-store = {
        enable = true;
        package = user.features.password-store.package;
        settings = {
          PASSWORD_STORE_DIR = "${user.features.xdg.baseDirs.stateHome}/password-store";
        };
      };
      programs.emacs = mkElispConfig {
        name = "ordenada-password-store";
        config = ''
          (eval-when-compile (require 'pass))
          (setopt pass-show-keybindings nil)
          (add-hook 'pass-mode-hook #'toggle-truncate-lines)
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "p" #'pass))

          (with-eval-after-load 'auth-source
            (auth-source-pass-enable))

          (autoload 'password-store-dir "password-store")
          (autoload 'password-store-list "password-store")
          (autoload 'consult--read "consult")

          (defun ordenada-password-store-consult (arg pass)
            "Interactively search the password store."
            (interactive
             (list current-prefix-arg
                   (consult--read (password-store-list)
                                  :prompt "Pass entry: "
                                  :sort nil
                                  :require-match nil
                                  :category 'pass)))
            (funcall (if arg
                         'password-store-url
                         'password-store-copy)
                     pass))

          (keymap-global-set "M-g P" #'ordenada-password-store-consult)

          (with-eval-after-load 'password-store
            (defvar ordenada-password-store-embark-actions
              (let ((map (make-sparse-keymap)))
                (keymap-set map "f" #'password-store-copy-field)
                (keymap-set map "b" #'password-store-url)
                (keymap-set map "e" #'password-store-edit)
                (keymap-set map "g" #'password-store-generate)
                (keymap-set map "r" #'password-store-rename)
                (keymap-set map "d" #'password-store-remove)
                (keymap-set map "i" #'password-store-insert)
                map)
              "Keymap for actions for pass entries."))

          (with-eval-after-load 'embark
            (require 'password-store)
            (add-to-list 'embark-keymap-alist
                         '(pass . ordenada-password-store-embark-actions)))
        '';
        elispPackages = with pkgs.emacsPackages; [
          pass
          password-store
          password-store-otp
        ];
      };
    });
  };
}
