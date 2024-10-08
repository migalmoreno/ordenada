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
          ${
            if initialNarrowing then
              ''
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
              ''
            else
              ""
          }
          (defun ordenada-goto-line-relative ()
            "Just a wrapper around `consult-goto-line', which uses
          relative line numbers, when narrowing is active."
            (interactive)
            (let ((consult-line-numbers-widen nil))
              (call-interactively #'consult-goto-line)))

          (define-key narrow-map (kbd "g") #'ordenada-goto-line-relative)

          (define-key minibuffer-local-map (kbd "M-r") #'consult-history)
          (define-key global-map (kbd "M-y") #'consult-yank-pop)
          (define-key goto-map (kbd "a") #'consult-org-agenda)
          (define-key goto-map (kbd "h") #'consult-org-heading)
          (define-key ctl-x-map "b" #'consult-buffer)
          (define-key help-map "a" #'consult-apropos)
          (define-key global-map (kbd "C-x C-r") #'consult-recent-file)
          (define-key ctl-x-map (kbd "M-:") #'consult-complex-command)
          (define-key ctl-x-4-map "b" #'consult-buffer-other-window)
          (let ((map goto-map))
            (define-key map (kbd "g") #'consult-goto-line)
            (define-key map (kbd "M-g") #'consult-goto-line)
            (define-key map (kbd "l") #'consult-line)
            (define-key map (kbd "o") #'consult-outline)
            (define-key map (kbd "i") #'consult-imenu)
            (define-key map (kbd "m") #'consult-mark)
            (define-key map (kbd "M") #'consult-global-mark)
            (define-key map (kbd "b") #'consult-bookmark))

          (let ((map search-map))
            (define-key map (kbd "f") #'consult-find)
            (define-key map (kbd "g") #'consult-ripgrep)
            (define-key map (kbd "e") #'consult-isearch-history)
            (define-key map (kbd "l") #'consult-line))

          (autoload 'consult-isearch-history "consult")
          (let ((map isearch-mode-map))
            (define-key map (kbd "M-e") #'consult-isearch-history)
            (define-key map (kbd "M-s e") #'consult-isearch-history)
            (define-key map (kbd "M-s l") #'consult-line))

          (with-eval-after-load 'consult
            (setq consult-narrow-key "C-=")
            (setq consult-widen-key "C--"))

          (with-eval-after-load 'embark
            (require 'embark-consult))

          (autoload 'consult-customize "consult" "" nil 'macro)
          (autoload 'consult--customize-set "consult")
          (with-eval-after-load 'consult
            (require 'embark-consult)
            (setq consult-ripgrep-args
                  (replace-regexp-in-string "^rg" "${pkgs.ripgrep}/bin/rg" consult-ripgrep-args))
            (consult-customize consult-buffer :preview-key "M-.")
            (consult-customize consult-history :category 'consult-history)
            (consult-customize consult-line :inherit-input-method t))

          (with-eval-after-load 'xref
            (setq xref-show-xrefs-function #'consult-xref))
        '';
        elispPackages = with pkgs.emacsPackages; [
          consult
          embark-consult
        ];
      };
    });
  };
}
