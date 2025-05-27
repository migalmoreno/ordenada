{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.vterm = {
      enable = lib.mkEnableOption "the Emacs vterm feature";
      package = lib.mkOption {
        type = lib.types.package;
        description = "The Vterm package to use.";
        default = pkgs.emacsPackages.vterm;
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.vterm" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-vterm";
        config = ''
          (keymap-global-set "s-t" #'vterm)
          ${mkIf (hasFeature "emacs.consult" user) ''
            (eval-when-compile (require 'cl-macs))
            (defun ordenada-vterm-consult-yank-pop-wrapper (orig-fun &rest args)
              "Use `vterm-insert' instead of `insert-for-yank' if
            `major-mode' is `vterm-mode'."
              (interactive "p")
              (if (equal major-mode 'vterm-mode)
                  (let ((inhibit-read-only t)
                        (yank-undo-function (lambda (_s _e) (vterm-undo))))
                    (cl-letf (((symbol-function 'insert-for-yank)
                               'vterm-insert))
                             (apply orig-fun args)))
                  (apply orig-fun args)))

             (advice-add 'consult-yank-pop :around #'ordenada-vterm-consult-yank-pop-wrapper)
          ''}
          ${mkIf (hasFeature "emacs.project" user) ''
            (defun ordenada-vterm-project-vterm ()
              "Start vterm in the current project's root directory.
            If a buffer already exists for running vterm in the project's root,
            switch to it.  Otherwise, create a new vterm buffer.
            With \\[universal-argument] prefix arg, create a new vterm buffer even
            if one already exists."
              (interactive)
              (let* ((default-directory (project-root (project-current t)))
                     (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
                     (vterm-buffer (get-buffer vterm-buffer-name)))
                (if (and vterm-buffer (not current-prefix-arg))
                    (pop-to-buffer-same-window vterm-buffer)
                    (vterm t))))

            (with-eval-after-load 'project
              (keymap-set project-prefix-map "t" #'ordenada-vterm-project-vterm))
          ''}
        '';
        elispPackages = [ user.features.emacs.vterm.package ];
      };
    });
  };
}
