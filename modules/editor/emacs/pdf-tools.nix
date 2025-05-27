{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkEnableOption;
in
{
  options = {
    ordenada.features.emacs.pdf-tools = {
      enable = mkEnableOption "the Emacs pdf-tools feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.pdf-tools" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-pdf-tools";
        config = with user.features.emacs.pdf-tools; ''
          (defgroup ordenada-pdf-tools nil
            "Custom tweaks for PDF Tools."
            :group 'ordenada)

          (defun ordenada-pdf-tools--list-buffers ()
            "List all currently-opened `pdf-view' mode buffers."
            (cl-remove-if-not
             (lambda (buffer)
               (with-current-buffer buffer
                 (derived-mode-p 'pdf-view-mode)))
             (buffer-list)))

          (defun ordenada-pdf-tools-update-buffers (&optional _theme)
            "Apply `ordenada-pdf-tools-mode' to currently open `pdf-view' mode buffers."
            (dolist (buffer (ordenada-pdf-tools--list-buffers))
              (with-current-buffer buffer
                (ordenada-pdf-tools-mode 1))))

           ${mkIf (hasFeature "emacs.modus-themes" user) ''
             (define-minor-mode ordenada-pdf-tools-mode
               "Apply `pdf-tools' settings based on the current theme."
               :group 'ordenada-pdf-tools
               (if ordenada-pdf-tools-mode
                   (if (ordenada-modus-themes--dark-theme-p)
                       (pdf-view-themed-minor-mode 1)
                     (pdf-view-themed-minor-mode -1))
                 (pdf-view-themed-minor-mode -1)))
             (add-hook 'pdf-view-mode-hook #'ordenada-pdf-tools-mode)
           ''}

           (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
           (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
           (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
           (autoload 'pdf-view-mode "pdf-view" "")
           (add-hook 'after-init-hook #'pdf-loader-install)
           (with-eval-after-load 'pdf-view
             (setq pdf-view-use-scaling t)
             (setq pdf-view-display-size 'fit-page)
             (setq pdf-view-resize-factor 1.025))
           (with-eval-after-load 'saveplace
             (require 'saveplace-pdf-view))
        '';
        elispPackages = with pkgs.emacsPackages; [
          pdf-tools
          saveplace-pdf-view
        ];
      };
    });
  };
}
