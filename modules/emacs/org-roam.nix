{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) types mkOption mkEnableOption;
in
{
  options = {
    ordenada.features.emacs.org-roam = {
      enable = mkEnableOption "Emacs Org Roam feature";
      captureTemplates = mkOption {
        type = types.lines;
        description = "The Org Roam capture templates";
        default = "";
      };
      directory = mkOption {
        type = types.str;
        description = "The Org Roam directory";
        default = "~/notes";
      };
      dailiesDirectory = mkOption {
        type = types.str;
        description = "The Org Roam dailies directory";
        default = "daily/";
      };
      dailiesCaptureTemplates = mkOption {
        type = types.lines;
        description = "The Org Roam dailies capture templates";
        default = "";
      };
      todoIntegration = mkEnableOption "todo integration in Org Roam.";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.org-roam" (user: {
      programs.emacs = pkgs.lib.ordenada.mkElispConfig {
        name = "ordenada-org-roam";
        config = with user.features.emacs.org-roam; ''
          (eval-when-compile
            (let ((org-roam-v2-ack t))
              (require 'org-roam)))
          (setq org-roam-v2-ack t)
          (setq org-roam-completion-everywhere t)
          (setq org-roam-directory "${directory}")
          (autoload 'org-roam-db-autosync-enable "org-roam")

          (let ((map mode-specific-map))
            (define-key map (kbd "n n") #'org-roam-buffer-toggle)
            (define-key map (kbd "n f") #'org-roam-node-find)
            (define-key map (kbd "n i") #'org-roam-node-insert)
            (define-key map (kbd "n r") #'org-roam-ref-find)
            (define-key map (kbd "n C") #'org-roam-capture))

          (with-eval-after-load 'org-roam
            (setq org-roam-db-location
                  (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                          "/emacs/org-roam.db"))
            (org-roam-db-autosync-enable)
            (cl-defmethod org-roam-node-type ((node org-roam-node))
              "Return the TYPE of NODE, where the TYPE is a directory of
          the node, relative to `org-roam-directory'."
              (condition-case
                  nil
                  (file-name-nondirectory
                   (directory-file-name
                    (file-name-directory
                     (file-relative-name (org-roam-node-file node)
                                         org-roam-directory))))
                (error "")))
            (setq org-roam-node-display-template
                  (concat "''${type:15} ''${title:80} " (propertize "''${tags:20}" 'face 'org-tag)))
            (setq org-roam-node-annotation-function
                  (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))
            ${
              if captureTemplates == "" then "" else ''(setq org-roam-capture-templates ${captureTemplates})''
            })

          (defun ordenada-org-roam-open-ref ()
            "Prompt you for a list of all ROAM_REFS in the current buffer."
            (interactive)
            (when (derived-mode-p 'org-mode)
              (if-let* ((refs (org-property-values "ROAM_REFS"))
                        (choices (mapcar
                                  (lambda (x)
                                    (org-unbracket-string "[[" "]]" x))
                                  (split-string
                                   (car (org-property-values "ROAM_REFS"))
                                   " ")))
                        (node-ref (completing-read
                                   "Refs: "
                                   (lambda (string pred action)
                                     (if (eq action 'metadata)
                                         `(metadata
                                           (category . org-roam-ref)
                                           ,(cons 'display-sort-function
                                                  'identity))
                                       (complete-with-action
                                        action choices string pred)))
                                   nil 'require-match)))
                  node-ref
                (error "No roam refs in this node"))))

          (with-eval-after-load 'org
            (let ((map org-mode-map))
              (define-key map (kbd "C-TAB") #'completion-at-point)
              (define-key map (kbd "C-c r r") #'org-roam-ref-add)
              (define-key map (kbd "C-c r R") #'org-roam-ref-remove)
              (define-key map (kbd "C-c r f") #'org-roam-ref-find)
              (define-key map (kbd "C-c r t") #'org-roam-tag-add)
              (define-key map (kbd "C-c r T") #'org-roam-tag-remove)
              (define-key map (kbd "C-c r a") #'org-roam-alias-add)
              (define-key map (kbd "C-c r A") #'org-roam-alias-remove)
              (define-key map (kbd "C-c r O") #'ordenada-org-roam-open-ref)))

          (with-eval-after-load 'window
            (add-to-list 'display-buffer-alist
                         `(,(rx "*org-roam*")
                           display-buffer-same-window)))

          (autoload 'org-roam-dailies-map "org-roam-dailies" "" nil 'keymap)
          (define-key mode-specific-map (kbd "d") #'org-roam-dailies-map)
          (with-eval-after-load 'org-roam-dailies
            (setq org-roam-dailies-directory "${dailiesDirectory}")
            ${
              if dailiesCaptureTemplates == "" then
                ""
              else
                ''(setq org-roam-dailies-capture-templates ${dailiesCaptureTemplates})''
            })

          ${
            if todoIntegration then
              ''
                (defun ordenada-org-roam-get-filetags ()
                  "Return the top-level tags for the current org-roam node."
                  (split-string
                   (or (cadr (assoc "FILETAGS"
                                    (org-collect-keywords '("filetags"))))
                       "")
                   ":" 'omit-nulls))

                (defun ordenada-org-roam-todo-p ()
                  "Return non-nil if the current buffer has any to-do entry."
                  (org-element-map
                      (org-element-parse-buffer 'headline)
                      'headline
                    (lambda (h)
                      (eq (org-element-property :todo-type h) 'todo))
                    nil 'first-match))

                (defun ordenada-org-roam-update-todo-tag ()
                  "Update the \"todo\" tag in the current buffer."
                  (when (and (not (active-minibuffer-window))
                             (org-roam-file-p))
                    (org-with-point-at 1
                      (let* ((tags (ordenada-org-roam-get-filetags))
                             (is-todo (ordenada-org-roam-todo-p)))
                        (cond ((and is-todo (not (member "todo" tags)))
                               (org-roam-tag-add '("todo")))
                              ((and (not is-todo) (member "todo" tags))
                               (org-roam-tag-remove '("todo"))))))))

                (defun ordenada-org-roam-list-todo-files ()
                  "Return a list of org-roam files containing the \"todo\" tag."
                  (org-roam-db-sync)
                  (let ((todo-nodes (cl-remove-if-not
                                     (lambda (n)
                                       (member "todo" (org-roam-node-tags n)))
                                     (org-roam-node-list))))
                    (delete-dups (mapcar 'org-roam-node-file todo-nodes))))

                (defun ordenada-org-roam-update-todo-files (&rest _)
                  "Update the value of `org-agenda-files'."
                  (setq org-agenda-files (ordenada-org-roam-list-todo-files)))

                (defun ordenada-org-roam-ref-add (ref node)
                  "Add REF to NODE.
                If NODE doesn't exist, create a new org-roam node with REF."
                  (interactive
                   (list
                    (read-string "Ref: ")
                    (org-roam-node-read)))
                  (if-let ((file (org-roam-node-file node)))
                      (with-current-buffer (or (find-buffer-visiting file)
                                               (find-file-noselect file))
                        (org-roam-property-add "ROAM_REFS" ref)
                        (save-buffer)
                        (kill-current-buffer))
                    (org-roam-capture-
                     :keys "r"
                     :node node
                     :info `(:ref ,ref)
                     :templates org-roam-capture-templates
                     :props '(:finalize find-file))))

                (with-eval-after-load 'org-roam
                  (add-hook 'org-roam-find-file-hook #'ordenada-org-roam-update-todo-tag)
                  (add-hook 'before-save-hook #'ordenada-org-roam-update-todo-tag))
                (advice-add 'org-agenda :before #'ordenada-org-roam-update-todo-files)
              ''
            else
              ""
          }
        '';
        elispPackages = with pkgs.emacsPackages; [ org-roam ];
      };
    });
  };
}
