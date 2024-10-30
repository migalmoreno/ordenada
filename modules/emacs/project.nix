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
      enable = lib.mkEnableOption "Emacs Project feature.";
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
            (require 'project)
            (require 'xdg))
          (defgroup ordenada-project nil
            "Custom `project.el' enhancements."
            :group 'ordenada)
          (defcustom ordenada-project-dominating-files '()
            "List of root files that indicate a directory is a project."
            :group 'ordenada-project
            :type '(repeat string))

          (defun ordenada-compilation-buffer-name (mode)
            "Returns the result of `project-prefixed-buffer-name' if inside
          project and `compilation--default-buffer-name' if not."
              (if (project-current)
                  (project-prefixed-buffer-name mode)
                  (compilation--default-buffer-name mode)))

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

          (defun ordenada-project-compile (&optional comint)
            "Compile current project and choose if buffer will be in COMINT mode."
            (interactive "P")
            (let ((default-directory (project-root (project-current t)))
                  (compilation-buffer-name-function
                   (or project-compilation-buffer-name-function
                       compilation-buffer-name-function)))
              (call-interactively 'compile nil (and comint (vector (list 4))))))

          (setopt ordenada-project-dominating-files '(${
            toString (map (x: ''"${x}"'') user.features.emacs.project.extraDominatingFiles)
          }))
          (add-hook 'project-find-functions #'ordenada-project-custom-root)
          (add-hook 'project-find-functions #'project-try-vc)
          (advice-add 'project-compile :override #'ordenada-project-compile)
          (with-eval-after-load 'project
            (with-eval-after-load 'consult
              (setq consult-project-root-function
                    (lambda ()
                      (when-let (project (project-current))
                        (car (project-roots project))))))
            (setq compilation-buffer-name-function #'ordenada-compilation-buffer-name)
            (setq project-compilation-buffer-name-function #'ordenada-compilation-buffer-name))
            (keymap-set project-prefix-map "F" #'consult-find)
            (keymap-set project-prefix-map "R" #'consult-ripgrep)
            (setopt project-switch-use-entire-map t)
            (setopt project-list-file (expand-file-name "emacs/projects" (xdg-cache-home))))
        '';
      };
    });
  };
}
