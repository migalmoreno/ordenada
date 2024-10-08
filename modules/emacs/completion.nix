{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.completion = {
      enable = lib.mkEnableOption "the Emacs completion feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.completion" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-completion";
        config = ''
          (defgroup ordenada-completion nil
            "Tweaks to the built-in Emacs completion."
            :group 'ordenada)

          (defun ordenada-completion-crm-indicator (args)
            "Display a discernible indicator for `completing-read-multiple'."
            (cons (concat "[CRM] " (car args)) (cdr args)))

          (advice-add 'completing-read-multiple :filter-args #'ordenada-completion-crm-indicator)

          (with-eval-after-load 'minibuffer
            (setq tab-always-indent 'complete)
            (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
            (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
            (setq completion-show-help nil)
            (setq completions-format 'one-column)
            (setq completions-header-format nil)
            (let ((map minibuffer-mode-map))
              (define-key map [remap next-line] #'minibuffer-next-completion)
              (define-key map [remap previous-line] #'minibuffer-previous-completion))
            (let ((map completion-in-region-mode-map))
              (define-key map (kbd "C-n") #'minibuffer-next-completion)
              (define-key map (kbd "C-p") #'minibuffer-previous-completion))
            (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

            (let ((map minibuffer-local-completion-map))
              (define-key map (kbd "SPC") nil)
              (define-key map (kbd "?") nil)))
        '';
        elispPackages = with pkgs.emacsPackages; [ kind-icon ];
      };
    });
  };
}
