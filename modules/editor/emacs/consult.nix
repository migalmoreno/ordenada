{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.consult = {
      enable = lib.mkEnableOption "the Emacs consult feature";
      initialNarrowing = lib.mkEnableOption "the initial narrowing of mini-buffer items";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.consult" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-consult";
        config = with user.features.emacs.consult; ''
          (eval-when-compile
            (require 'consult))
          (defcustom ordenada-consult nil
            "Tweaks to Consult configuration."
            :group 'ordenada)
          ${mkIf initialNarrowing ''
            (defcustom ordenada-completion-initial-narrow-alist '()
              "Alist of MODE . KEY to present an initial completion narrowing via
                          `consult'."
              :group 'ordenada-consult
              :type 'list)

            (defun ordenada-completion--mode-buffers (&rest modes)
              "Return a list of buffers that are derived from MODES in `buffer-list'."
              (cl-remove-if-not
               (lambda (buffer)
                 (with-current-buffer buffer
                   (cl-some 'derived-mode-p modes)))
               (buffer-list)))

            (defun ordenada-completion-initial-narrow ()
              "Set initial narrow source for buffers under a specific mode."
              (let* ((buffer-mode-assoc ordenada-completion-initial-narrow-alist)
                     (key (and (eq this-command 'consult-buffer)
                               (or (alist-get
                                    (buffer-local-value
                                     'major-mode
                                     (window-buffer (minibuffer-selected-window)))
                                    buffer-mode-assoc)
                                   (cdr (cl-find-if
                                         (lambda (mode)
                                           (with-current-buffer
                                               (window-buffer (minibuffer-selected-window))
                                             (derived-mode-p (car mode))))
                                         buffer-mode-assoc))))))
                (when key
                  (setq unread-command-events
                        (append unread-command-events (list key 32))))))

            (add-hook 'minibuffer-setup-hook #'ordenada-completion-initial-narrow)
          ''}
          (defun ordenada-goto-line-relative ()
            "Just a wrapper around `consult-goto-line', which uses
          relative line numbers, when narrowing is active."
            (interactive)
            (let ((consult-line-numbers-widen nil))
              (call-interactively #'consult-goto-line)))

          (keymap-set narrow-map "g" #'ordenada-goto-line-relative)

          (keymap-set minibuffer-local-map "M-r" #'consult-history)
          (keymap-global-set "M-y" #'consult-yank-pop)
          (keymap-set goto-map "a" #'consult-org-agenda)
          (keymap-set goto-map "h" #'consult-org-heading)
          (keymap-set ctl-x-map "b" #'consult-buffer)
          (keymap-set help-map "a" #'consult-apropos)
          (keymap-global-set "C-x C-r" #'consult-recent-file)
          (keymap-set ctl-x-map "M-:" #'consult-complex-command)
          (keymap-set ctl-x-4-map "b" #'consult-buffer-other-window)
          (let ((map goto-map))
            (keymap-set map "g" #'consult-goto-line)
            (keymap-set map "M-g" #'consult-goto-line)
            (keymap-set map "l" #'consult-line)
            (keymap-set map "o" #'consult-outline)
            (keymap-set map "i" #'consult-imenu)
            (keymap-set map "m" #'consult-mark)
            (keymap-set map "M" #'consult-global-mark)
            (keymap-set map "b" #'consult-bookmark))

          (let ((map search-map))
            (keymap-set map "f" #'consult-find)
            (keymap-set map "g" #'consult-ripgrep)
            (keymap-set map "e" #'consult-isearch-history)
            (keymap-set map "l" #'consult-line))

          (autoload 'consult-isearch-history "consult")
          (let ((map isearch-mode-map))
            (keymap-set map "M-e" #'consult-isearch-history)
            (keymap-set map "M-s e" #'consult-isearch-history)
            (keymap-set map "M-s l" #'consult-line))

          (with-eval-after-load 'consult
            (setopt consult-narrow-key "C-=")
            (setopt consult-widen-key "C--"))

          (with-eval-after-load 'embark
            (require 'embark-consult))

          (autoload 'consult-customize "consult" "" nil 'macro)
          (autoload 'consult--customize-set "consult")
          (with-eval-after-load 'consult
            (require 'embark-consult)
            (setopt consult-ripgrep-args
                    (replace-regexp-in-string "^rg" "${pkgs.ripgrep}/bin/rg" consult-ripgrep-args))
            (consult-customize consult-buffer :preview-key "M-.")
            (consult-customize consult-history :category 'consult-history)
            (consult-customize consult-line :inherit-input-method t))

          (with-eval-after-load 'xref
            (setopt xref-show-xrefs-function #'consult-xref))
        '';
        elispPackages = with pkgs.emacsPackages; [
          consult
          embark-consult
        ];
      };
    });
  };
}
