{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.compile = {
      enable = lib.mkEnableOption "the compile feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "compile" (user: {
      home.packages = with pkgs; [ gnumake ];
      programs.emacs = mkElispConfig {
        name = "ordenada-compile";
        config = ''
          (eval-when-compile
            (require 'project))
          (defun ordenada-compilation-buffer-name (mode)
            "Returns the result of `project-prefixed-buffer-name' if inside
          project and `compilation--default-buffer-name' if not."
              (if (project-current)
                  (project-prefixed-buffer-name mode)
                  (compilation--default-buffer-name mode)))

          (defun ordenada-project-compile (&optional comint)
            "Compile current project and choose if buffer will be in COMINT mode."
            (interactive "P")
            (let ((default-directory (project-root (project-current t)))
                  (compilation-buffer-name-function
                   (or project-compilation-buffer-name-function
                       compilation-buffer-name-function)))
              (call-interactively 'compile nil (and comint (vector (list 4))))))

          (defun ordenada-compile-ansi-color-apply ()
            "Translate control sequences into text properties in compile buffer."
            (interactive)
            (ansi-color-apply-on-region (point-min) (point-max)))

          (add-hook 'compilation-filter-hook #'ordenada-compile-ansi-color-apply)
          (with-eval-after-load 'compile
            (setq compilation-buffer-name-function #'ordenada-compilation-buffer-name))

          (advice-add 'project-compile :override #'ordenada-project-compile)
          (with-eval-after-load 'project
            (setopt project-compilation-buffer-name-function #'ordenada-compilation-buffer-name))
        '';
        elispPackages = with pkgs.emacsPackages; [ envrc ];
      };
    });
  };
}
