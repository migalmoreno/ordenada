{
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "evil"
  ];
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-evil";
        config = # elisp
          ''
            (defun ordenada-evil-hook ()
              (dolist (mode '(custom-mode
                              eshell-mode
                              git-rebase-mode
                              term-mode))
                      (add-to-list 'evil-emacs-state-modes mode)))
            (eval-when-compile
             (require 'evil)
             (require 'evil-collection)
             (require 'evil-commentary)
             (require 'evil-surround))
            (setq evil-want-keybinding nil)
            (with-eval-after-load 'evil-autoloads
              (evil-mode 1))
            (with-eval-after-load 'evil-collection-autoloads
              (evil-collection-init))
            (with-eval-after-load 'evil-commentary-autoloads
              (evil-commentary-mode))
            (with-eval-after-load 'evil-surround-autoloads
              (global-evil-surround-mode 1))
            (setq evil-want-integration t)
            (setq evil-want-C-u-scroll t)
            (setq evil-want-C-i-jump nil)
            (setq evil-respect-visual-line-mode t)
            (setq evil-undo-system 'undo-fu)
            (setq evil-want-fine-undo t)
            ;; Since =evil-mode= take over =C-u= for buffer scrolling,
            ;; the =universal-argument= command needs to be rebind to another key
            ;; sequence, here =C-M-u=.
            (global-unset-key (kbd "C-M-u"))
            (global-unset-key (kbd "C-u"))
            (global-set-key (kbd "C-M-u") 'universal-argument)
            (global-set-key (kbd "C-u") 'evil-scroll-up)
            ;; Keybinding preferences
            (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
            (global-set-key (kbd "<lwindow-j>") 'ignore)
            (global-set-key (kbd "<lwindow-k>") 'ignore)
            (setq evil-shift-width tab-width)
            ;; use evil in minibuffers with Ctrl key.
            ;; these provide a lighter and less intrusive minibuffer experience
            ;; than (setq evil-want-minibuffer t)
            (let ((map minibuffer-mode-map))
              (define-key map (kbd "C-j") 'next-line-or-history-element)
              (define-key map (kbd "C-k") 'previous-line-or-history-element)
              (define-key map (kbd "C-r") 'consult-history))
            (with-eval-after-load
                'evil
                (evil-define-command ordenada-evil-open-below-comment-aware (&optional count)
                  "Like `evil-open-below`, but if inside a documentation type comment, continue it."
                  (interactive "p")
                  (let* ((line-length (- (line-end-position) (line-beginning-position)))
                        (inside-comment (save-excursion
                                          (end-of-line)
                                          (nth 4 (syntax-ppss))))
                        (block-start (save-excursion
                          (beginning-of-line)
                          (if (looking-at "\\(?: \\*\\|/\\*\\*\\|/\\*\\)")
                             (match-string 0)
                           nil)))
                        (block-end (save-excursion
                          (end-of-line)
                          (when (>= line-length 2)
                            (backward-char 2))
                          (if (looking-at "\\(?:\\*/\\)")
                             t
                           nil)))
                        (i (or count 1)))
                    (dotimes (x i)
                      (evil-open-below 1)
                      (when (and inside-comment (stringp block-start) (not block-end))
                        (beginning-of-line)
                        (insert " * ")))))
              (define-key evil-normal-state-map (kbd "o") #'ordenada-evil-open-below-comment-aware)
              (add-hook 'evil-mode-hook 'ordenada-evil-hook)
              (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
              (define-key evil-insert-state-map
                (kbd "C-h") 'evil-delete-backward-char-and-join)
              ;; Use visual line motions even outside of visual-line-mode buffers
              (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
              (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
              (define-key evil-operator-state-map (kbd "j") 'evil-next-line)
              (define-key evil-operator-state-map (kbd "k") 'evil-previous-line)
              ;; undo and redo
              (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
              (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo)
              (evil-define-key '(normal insert visual)
                               org-mode-map (kbd "C-j") 'org-next-visible-heading)
              (evil-define-key '(normal insert visual)
                               org-mode-map (kbd "C-k")
                               'org-previous-visible-heading)
              (evil-define-key '(normal insert visual)
                               org-mode-map (kbd "M-j") 'org-metadown)
              (evil-define-key '(normal insert visual)
                               org-mode-map (kbd "M-k") 'org-metaup)
              (evil-set-initial-state 'messages-buffer-mode 'normal)
              (evil-set-initial-state 'dashboard-mode 'normal)
              ;; Is this a bug in evil-collection?
              (setq evil-collection-company-use-tng nil)
              (setq evil-collection-outline-bind-tab-p nil)
              (with-eval-after-load
                  'winner
                (let ((map evil-window-map))
                  (define-key map (kbd "u") 'winner-undo)
                  (define-key map (kbd "U") 'winner-redo))))
            (with-eval-after-load
                'evil-collection
              (setq evil-collection-mode-list
                    (remove 'lispy evil-collection-mode-list)))
            (add-hook 'org-mode-hook 'evil-org-mode)
            (add-hook 'org-agenda-mode-hook 'evil-org-mode)
            (with-eval-after-load
                'org
              (add-hook 'evil-org-mode-hook
                        (lambda ()
                          (evil-org-set-key-theme
                           '(navigation todo insert textobjects additional))))
              (with-eval-after-load
                  'evil-org
                (require 'evil-org-agenda)
                (evil-org-agenda-set-keys)))
          '';
        elispPackages = with pkgs.emacsPackages; [
          evil
          evil-collection
          evil-org
          evil-commentary
          evil-surround
          undo-fu
        ];
      };
    };
}
