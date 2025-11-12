{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "browse-url"
  ];
  options.mappings = lib.mkOption {
    type = lib.types.attrs;
    description = "Set of mappings of original URL to redirect URL";
    default = { };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-browse-url";
        config =
          with config.ordenada.features.emacs; # elisp
          ''
            (eval-when-compile
              (require 'cl-lib))

            (defgroup ordenada-browse-url nil
              "Generic utilities to enhance `browse-url'."
              :group 'rde)

            (defcustom ordenada-browse-url-mappings '()
              "URL mapping alist.
            It has the form (SERVICE . ALT) where SERVICE is the original hostname of
            the service and ALT is the alternative service host to rewrite urls to, and
            viceversa."
              :type 'list
              :group 'ordenada-browse-url)

            (cl-defun ordenada-browse-url--transform-url (url &key (alt t))
              "Transform URL to its mapping in `rde-browse-url-mappings'.
            If ALT is non-nil, URL is assumed to be an alternative so the logic is reversed."
              (string-match (rx (group (+ any) "://" (* (not "/"))) (* any)) url)
              (let* ((service-url (match-string 1 url))
                     (mapping (if alt
                                  (cl-rassoc service-url rde-browse-url-mappings
                                             :test 'string=)
                                (assoc-string service-url
                                              rde-browse-url-mappings))))
                (if mapping
                    (if alt
                        (replace-regexp-in-string
                         service-url
                         (car mapping)
                         url)
                      (replace-regexp-in-string
                       service-url
                       (cdr mapping)
                       url))
                  url)))

            (defun ordenada-browse-url-bookmark-make-record (url title)
              "Create a bookmark record from a browser buffer with URL and TITLE."
              (let* ((defaults (delq nil (list title url)))
                     (bookmark
                      `(,title
                        ,@(bookmark-make-record-default 'no-file)
                        ,(cons 'browser-url url)
                        ,(cons 'filename url)
                        ,(cons 'handler 'rde-browse-url-bookmark-jump)
                        ,(cons 'defaults defaults))))
                bookmark))

            (defun ordenada-browse-url-bookmark-jump (bookmark)
              "Jump to BOOKMARK in the default browser."
              (let ((location (bookmark-prop-get bookmark 'browser-url)))
                (browse-url-default-browser location)))

            (defun ordenada-browse-url-alt-bookmark-jump (bookmark)
              "Jump to BOOKMARK in an alternative browser."
              (cl-letf (((symbol-function 'browse-url-can-use-xdg-open) 'ignore))
                (ordenada-browse-url-bookmark-jump bookmark)))

            ${lib.optionalString embark.enable ''
              (defun ordenada-browse-url-open-with-cookies (cookies &optional url)
                "Open URL with COOKIES in corresponding external application."
                (interactive "\nsURL: ")
                (let ((url-request-extra-headers
                       `(("Cookie"
                          ,(cl-loop for (field cookie) in cookies
                                    collect (format " %s=%s;" field cookie)
                                    into headers
                                    finally (return (string-join headers))))))
                      (filename (concat temporary-file-directory
                                        (car (last (split-string url "/"))))))
                  (unless (file-exists-p filename)
                    (with-current-buffer
                        (url-retrieve-synchronously url t)
                      (goto-char (point-min))
                      (re-search-forward "^$")
                      (forward-line 1)
                      (delete-region (point) (point-min))
                      (write-region (point-min) (point-max) filename)))
                  (embark-open-externally filename)))

                  (with-eval-after-load 'embark
                    (keymap-set embark-bookmark-map "c"
                                #'ordenada-browse-url-alt-bookmark-jump))
            ''}

            (defun ordenada-browse-url-add-scheme (fun url &rest args)
              "Add https scheme to URL if missing and invoke FUN and ARGS with it."
              (let ((link (if (string-match (rx bol (+ (in (?A . ?Z))) ":") url)
                              url
                            (concat "https:" url))))
                (apply fun link args)))

            (defun ordenada-browse-url-trace-url (fun url &rest args)
              "Transform URL to its original form and invoke FUN and ARGS with it."
              (let ((link (rde-browse-url--transform-url url)))
                (apply fun link args)))

            (setopt ordenada-browse-url-mappings ${ordenada-lib.elisp.toAlist browse-url.mappings})

            (advice-add 'browse-url-xdg-open :around 'ordenada-browse-url-add-scheme)

            (with-eval-after-load 'browse-url
              (setq browse-url-browser-function 'browse-url-xdg-open))
          '';
      };
    };
}
