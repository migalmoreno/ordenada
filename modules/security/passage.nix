{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "passage";
  options =
    { config, pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "passage" { };
      identitiesFile = lib.mkOption {
        type = lib.types.str;
        default = "${config.ordenada.features.xdg.baseDirs.stateHome}/passage/identities";
        description = "The identities file to be used by passage.";
      };
    };
  homeManager =
    { config, pkgs, ... }:
    let
      passage = pkgs.emacsPackages.melpaBuild {
        pname = "passage";
        version = "2.0";
        src = pkgs.fetchFromGitHub {
          owner = "anticomputer";
          repo = "passage.el";
          rev = "5f1ad815464b1e4ce7880b835f9e805a8b9b15a4";
          hash = "sha256-ENnleP9LYHUsiRO+pwd0bz1ITlb2C+xtzXhYM15UXYs=";
        };
        packageRequires = with pkgs.emacsPackages; [
          f
          s
          with-editor
        ];
      };
    in
    lib.mkIf (!config.ordenada.features.password-store.enable) {
      programs.password-store = with config.ordenada.features.passage; {
        inherit package;
        enable = lib.mkDefault true;
        settings = {
          PASSAGE_DIR = "${config.ordenada.features.xdg.baseDirs.stateHome}/passage/store";
          PASSAGE_IDENTITIES_FILE = identitiesFile;
        };
      };
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-passage";
        config = # elisp
          ''
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
        elispPackages = [
          passage
        ];
      };
    };
}
