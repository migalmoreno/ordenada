{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "nyxt"
    "emacs"
  ];
  homeManager =
    { config, pkgs, ... }:
    {
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-emacs";
        config =
          with config.ordenada.features; # lisp
          ''
            (define-configuration (web-buffer
                                   prompt-buffer
                                   nyxt/mode/editor:editor-buffer)
                ((default-modes `(nyxt/mode/emacs:emacs-mode ,@%slot-value%))))

            ${lib.optionalString emacs.enable ''
              (defun eval-in-emacs (&rest s-exps)
                "Evaluate S-EXPS with `emacsclient'."
                (let ((s-exps-string (str:replace-all
                                      "nyxt::" "" (write-to-string
                                                   `(progn
                                                      (setq print-length nil)
                                                      ,@s-exps)
                                                   :case :downcase))))
                  (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
                  (uiop:run-program
                   (list "emacsclient" "-e" s-exps-string)
                   :output '(:string :stripped t))))
            ''}

            ${lib.optionalString emacs.org-roam.enable ''
              (define-command org-roam-ref-capture ()
                "Reference an org-roam node with the current page."
                (eval-in-emacs
                 `(ordenada-org-roam-ref-add ,(render-url (url (current-buffer)))
                                             (org-roam-node-read))))
            ''}

            ${lib.optionalString emacs.browse-url.enable ''
              (define-command save-as-emacs-bookmark ()
                "Save current page in Nyxt as a bookmark record in Emacs."
                (eval-in-emacs
                 `(let ((bookmark-make-record-function
                          (lambda ()
                            (ordenada-browse-url-bookmark-make-record
                             ,(quri:render-uri (url (current-buffer)))
                             ,(title (current-buffer))))))
                    (bookmark-set)))
                (echo "Bookmark stored"))
            ''}

            ${lib.optionalString git.enable ''
              (define-command-global magit-clone ()
                "Clone and open the current repository with Magit."
                (let ((path (sera:path-join
                             (user-homedir-pathname)
                             "src/"
                             (car (last
                                   (str:split
                                    "/" (quri:uri-path
                                         (url (current-buffer)))))))))
                  (if (uiop:directory-exists-p path)
                      (echo "Error: Directory ~a already exists."
                            (namestring path))
                      (eval-in-emacs
                       '(require 'magit)
                       `(magit-clone-internal
                         ,(quri:render-uri (url (current-buffer)))
                         ,(namestring path)
                         nil)))))
            ''}

            ${lib.optionalString (mpv.enable && yt-dlp.enable) ''
              (defun play-emacs-mpv (url &rest extra-args &key &allow-other-keys)
                "Play stream from URL with EXTRA-ARGS in an Emacs mpv process."
                (let* ((nyxt::*interactive-p* t)
                       (url (render-url (quri:uri url)))
                       (playlist (null (member (eval-in-emacs
                                                '(progn
                                                  (require 'mpv)
                                                  (mpv-get-property "playlist")))
                                               '("nil" "[]") :test 'string=)))
                       (play-or-enqueue
                         (when playlist
                           (nyxt:prompt1
                            :prompt "Select"
                            :sources (make-instance
                                      'prompter:yes-no-source
                                      :constructor '("Play" "Enqueue")))))
                       (formats (delete-duplicates
                                 (remove-if-not
                                  (lambda (f)
                                    (ppcre:scan "\\d+x\\d+" f))
                                  (mapcar (lambda (f) (getf f :resolution))
                                          (with-input-from-string
                                              (s (eval-in-emacs
                                                  `(progn
                                                     (require 'ytdl)
                                                     (ytdl--list-formats ,url))))
                                            (read s))))
                                 :test 'equal))
                       (format (when formats
                                 (ppcre:register-groups-bind
                                  (height)
                                  ("\\d+x(\\d+)"
                                   (nyxt:prompt1
                                    :prompt "Format"
                                    :sources (make-instance
                                              'prompter:source
                                              :name "Formats"
                                              :constructor formats)))
                                  (format nil "best[height<=~a]" height))))
                       (res (and play-or-enqueue
                                 (string= play-or-enqueue "Enqueue"))))
                  (eval-in-emacs
                   `(apply 'ordenada-mpv-play-url ,url ,format
                           :select nil :playlist ,res ',extra-args))))

                  (define-command play-emacs-mpv-current (&optional (buffer (current-buffer)))
                    "Play contents of BUFFER in an Emacs-controlled mpv process."
                    (play-emacs-mpv (render-url (url buffer))))
            ''}
          '';
      };
    };
}
