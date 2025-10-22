{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "password-store";
  options =
    { pkgs, ... }:
    {
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.pass-wayland.withExtensions (exts: [ exts.pass-otp ]);
        description = "The package to use for password-store.";
      };
    };
  homeManager =
    { config, pkgs, ... }:
    lib.mkIf (!config.ordenada.features.passage.enable) {
      programs.password-store = {
        enable = true;
        package = config.ordenada.features.password-store.package;
        settings = {
          PASSWORD_STORE_DIR = "${config.ordenada.features.xdg.baseDirs.stateHome}/password-store";
        };
      };
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
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
    };
}
