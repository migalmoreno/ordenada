{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) types mkOption mkEnableOption;
  passage-el = (
    pkgs.emacsPackages.trivialBuild {
      pname = "passage";
      version = "2.0";
      src = pkgs.fetchFromGitHub {
        owner = "anticomputer";
        repo = "passage.el";
        rev = "5f1ad815464b1e4ce7880b835f9e805a8b9b15a4";
        hash = "sha256-ENnleP9LYHUsiRO+pwd0bz1ITlb2C+xtzXhYM15UXYs=";
      };
      propagatedBuildInputs = with pkgs.emacsPackages; [
        f
        s
        with-editor
      ];
    }
  );
in
{
  options = {
    ordenada.features.passage = {
      enable = mkEnableOption "the passage feature";
      package = mkOption {
        type = types.package;
        default = pkgs.passage;
        description = "The package to use for passage.";
      };
      identitiesFile = mkOption {
        type = types.str;
        default = "${config.ordenada.features.xdg.baseDirs.stateHome}/passage/identities";
        description = "The identities file to be used by passage.";
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "passage" (user: {
      programs.password-store = with user.features.passage; {
        inherit package;
        enable = true;
        settings = {
          PASSAGE_DIR = "${user.features.xdg.baseDirs.stateHome}/passage/store";
          PASSAGE_IDENTITIES_FILE = identitiesFile;
        };
      };
      programs.emacs = mkElispConfig {
        name = "ordenada-passage";
        config = ''
          (eval-and-compile (require 'passage))
          (setopt passage-show-keybindings nil)
          (add-hook 'passage-mode-hook #'toggle-truncate-lines)
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "p" #'passage))

          (with-eval-after-load 'auth-source
            (auth-source-passage-enable))

          (autoload 'passage-store-dir "passage")
          (autoload 'passage-store-list "passage")
          (autoload 'consult--read "consult")

          (defun ordenada-passage-store-consult (arg pass)
            "Interactively search the passage store."
            (interactive
             (list current-prefix-arg
                   (consult--read (passage-store-list)
                                  :prompt "Pass entry: "
                                  :sort nil
                                  :require-match nil
                                  :category 'pass)))
            (funcall (if arg
                         #'passage-store-url
                         #'passage-store-copy)
                     pass))

          (keymap-global-set "M-g P" #'ordenada-passage-store-consult)

          (with-eval-after-load 'passage
            (defvar ordenada-passage-store-embark-actions
              (let ((map (make-sparse-keymap)))
                (keymap-set map "f" #'passage-store-copy-field)
                (keymap-set map "b" #'passage-store-url)
                (keymap-set map "e" #'passage-store-edit)
                (keymap-set map "g" #'passage-store-generate)
                (keymap-set map "r" #'passage-store-rename)
                (keymap-set map "d" #'passage-store-remove)
                (keymap-set map "i" #'passage-store-insert)
                map)
              "Keymap for actions for passage store entries."))

          (with-eval-after-load 'embark
            (require 'passage)
            (add-to-list 'embark-keymap-alist
                         '(pass . ordenada-passage-store-embark-actions)))
        '';
        elispPackages = [ passage-el ];
      };
    });
  };
}
