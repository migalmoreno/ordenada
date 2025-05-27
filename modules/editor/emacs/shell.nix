{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.shell = {
      enable = lib.mkEnableOption "Emacs shell feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.shell" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-shell";
        config = ''
          (with-eval-after-load 'sh-script
            (setopt sh-basic-offset 2)
            (setopt sh-indent-comment nil)
            (setopt sh-first-lines-indent nil))
          (add-to-list 'display-buffer-alist
                        `(,(rx "*Async Shell Command" (* any) "*")
                          (display-buffer-no-window)))
          (with-eval-after-load 'org
            (add-to-list 'org-structure-template-alist
                         '("sh" . "src sh")))
          (with-eval-after-load 'ob-core
            (require 'ob-shell))
          (with-eval-after-load 'project
            (keymap-set project-prefix-map "s" #'project-shell)
            (add-to-list 'project-switch-commands
                         '(project-shell "Start an inferior shell")))
        '';
      };
    });
  };
}
