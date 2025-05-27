{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.project = {
      enable = lib.mkEnableOption "Emacs Project feature";
      extraDominatingFiles = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of extra dominating files used to identify projects.";
        default = [
          ".project.el"
          ".dir-locals.el"
          ".gitignore"
        ];
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.project" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-project";
        config = ''
          (eval-when-compile
            (require 'xdg))
          (defgroup ordenada-project nil
            "Custom `project.el' enhancements."
            :group 'ordenada)
          (defcustom ordenada-project-dominating-files '()
            "List of root files that indicate a directory is a project."
            :group 'ordenada-project
            :type '(repeat string))

          (defun ordenada-project-custom-root (dir)
            "Search in project's DIR for a set of project dominating files."
            (let* ((files ordenada-project-dominating-files)
                   (root (cl-find-if (lambda (file)
                                       (locate-dominating-file dir file))
                                     files)))
              (when root
                (cons 'explicit (locate-dominating-file dir root)))))

          (cl-defmethod project-root ((project (head explicit)))
            "Determine the PROJECT root."
            (cdr project))

          (setopt ordenada-project-dominating-files
                  ${mkList user.features.emacs.project.extraDominatingFiles})
          (add-hook 'project-find-functions #'project-try-vc -90)
          (add-hook 'project-find-functions #'ordenada-project-custom-root 50)

          (with-eval-after-load 'project
            (keymap-set project-prefix-map "F" #'consult-find)
            (keymap-set project-prefix-map "R" #'consult-ripgrep)
            (setopt project-switch-use-entire-map t)
            (setopt project-list-file
                   (expand-file-name "emacs/projects" (xdg-cache-home))))
        '';
      };
    });
  };
}
