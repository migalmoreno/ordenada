{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "emms"
  ];
  options = {
    infoMethod = lib.mkOption {
      type = lib.types.enum [ "libtag" ];
      description = "The EMMS info method to use to retrieve information about tracks.";
      default = "libtag";
    };
    key = lib.mkOption {
      type = lib.types.str;
      description = "Keybinding for EMMS map operations.";
      default = "E";
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-emms";
        config =
          with config.ordenada.features; # elisp
          ''
            (eval-when-compile
              (require 'emms))
            (defvar ordenada-emms-map nil
              "Map to bind `emms' commands under.")
            (define-prefix-command 'ordenada-emms-map)

            (defun ordenada-emms-source-playlist ()
              "Load a playlist with media files from emms' default directory."
              (interactive)
              (require 'emms)
              (ignore-errors
                (if (not emms-playlist-buffer)
                    (progn
                      (emms-add-directory-tree emms-source-file-default-directory)
                      (emms-playlist-mode-go))
                  (with-current-emms-playlist
                   (unless (emms-playlist-selected-track)
                     (emms-add-directory-tree
                      emms-source-file-default-directory))
                   (emms-playlist-mode-go)))))

            (defun ordenada-emms-source-track (url title &optional length play)
              "Append or PLAY track with URL, TITLE, and LENGTH to the playlist."
              (interactive "sURL: \nsTitle: \nP")
              (if length
                  (emms-add-ordenada-emms-track url title length)
                (emms-add-ordenada-emms-track url title nil))
              (when play
                (emms-stop)
                (emms-playlist-current-select-last)
                (emms-start)))

            (defun ordenada-emms-toggle-random-repeat ()
              "Toggle the random and repeat state for the current EMMS playlist."
              (interactive)
              (emms-toggle-random-playlist)
              (if (and emms-repeat-track emms-random-playlist)
                  (progn
                    (setq emms-repeat-track nil)
                    (message "Will play tracks randomly and repeat the track"))
                (setq emms-repeat-track t)
                (message "Will play tracks sequentially and repeat the track")))

            (defun ordenada-emms-seek-to-beginning ()
              "Seek to beginning of current EMMS track."
              (interactive)
              (emms-seek-to 0))

            (defun ordenada-emms-next ()
              "Move to the next track depending on the current playlist state."
              (interactive)
              (if emms-random-playlist
                  (emms-random)
                (emms-next)))

            (defun ordenada-emms-previous ()
              "Move to the previous track based on the current playlist state."
              (interactive)
              (if emms-random-playlist
                  (emms-random)
                (emms-previous)))

            (define-emms-source ordenada-emms-track (url title &optional length)
              (let ((emms-track (emms-track 'url url)))
                (emms-track-set emms-track 'info-title title)
                (when length
                  (emms-track-set emms-track 'info-playing-time length))
                (emms-playlist-insert-track emms-track)))

            ${lib.optionalString yt-dlp.enable ''
               (eval-when-compile
                (require 'ytdl))

              (with-eval-after-load 'emms-playlist-mode
                (defun ordenada-emms-download-track ()
                  "Download EMMS track at point using `ytdl'."
                  (interactive)
                  (emms-playlist-ensure-playlist-buffer)
                  (with-current-emms-playlist
                    (let* ((dl-type (ytdl--get-download-type))
                           (track (emms-playlist-track-at))
                           (title (emms-track-get track 'info-title))
                           (source (emms-track-get track 'name)))
                      (if (equal (emms-track-get track 'type) 'url)
                          (ytdl--download-async
                           source
                           (expand-file-name
                            title (ytdl--eval-field (nth 1 dl-type)))
                           (ytdl--eval-list
                            (ytdl--eval-field (nth 2 dl-type)))
                           'ignore
                           (car dl-type))
                        (error "Track `%s' is not a remote track to download"
                               title)))))
                (keymap-set emms-playlist-mode-map "m"
                            #'ordenada-emms-download-track))
            ''}

            ${lib.optionalString emacs.dired.enable ''
              (with-eval-after-load 'dired
                (define-key dired-mode-map "e" 'emms-play-dired))
            ''}

            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${emacs.emms.key}" #'ordenada-emms-map))

            (let ((map ordenada-emms-map))
              (keymap-set map "b" #'emms-smart-browse)
              (keymap-set map "h" #'emms-history-save)
              (keymap-set map "q" #'emms-stop)
              (keymap-set map "s" #'emms-toggle-random-playlist)
              (keymap-set map "t" #'emms-seek-to)
              (keymap-set map "SPC" #'emms-pause)
              (keymap-set map "r" #'emms-toggle-repeat-track)
              (keymap-set map "R" #'emms-toggle-repeat-playlist)
              (keymap-set map "d" #'ordenada-emms-toggle-random-repeat)
              (keymap-set map "l" #'ordenada-emms-source-playlist)
              (keymap-set map "n" #'ordenada-emms-next)
              (keymap-set map "p" #'ordenada-emms-previous)
              (keymap-set map "a" #'ordenada-emms-seek-to-beginning))

            (with-eval-after-load 'emms
              (require 'emms-setup)
              (require 'xdg)
              (require 'env)
              (require 'emms-info-${emacs.emms.infoMethod})
              (emms-all)

              ${lib.optionalString mpv.enable ''
                (defun ordenada-emms-mpv-kill ()
                  "Kill mpv process unless it's `emms-player-mpv-proc'."
                  (interactive)
                  (require 'mpv)
                  (require 'emms-player-mpv)
                  (when (equal mpv--process
                               emms-player-mpv-proc)
                    (emms-stop))
                  (when mpv--queue
                    (tq-close mpv--queue))
                  (when (and (mpv-live-p)
                             (not (equal mpv--process
                                         emms-player-mpv-proc)))
                    (kill-process mpv--process))
                  (with-timeout
                      (0.5 (error "Failed to kill mpv"))
                    (while (and (mpv-live-p)
                                (not (equal mpv--process
                                            emms-player-mpv-proc)))
                      (sleep-for 0.05)))
                  (setq mpv--process nil)
                  (setq mpv--queue nil)
                  (run-hooks 'mpv-finished-hook))

                (defun ordenada-emms-connect-to-mpv-proc ()
                  "Connect to a running emms mpv process."
                  (interactive)
                  (require 'mpv)
                  (setq mpv-playing-time-string "")
                  (when (not (equal mpv--process
                                    emms-player-mpv-proc))
                    (mpv-kill))
                  (setq mpv--process emms-player-mpv-proc)
                  (set-process-query-on-exit-flag mpv--process nil)
                  (set-process-sentinel
                   mpv--process
                   (lambda (p _e)
                     (when (memq (process-status p) '(exit signal))
                       (when (not (equal mpv--process
                                         emms-player-mpv-proc))
                         (mpv-kill))
                       (run-hooks 'mpv-on-exit-hook))))
                  (unless mpv--queue
                    (setq mpv--queue
                          (tq-create
                           (make-network-process
                            :name "emms-mpv-socket"
                            :family 'local
                            :service emms-player-mpv-ipc-socket
                            :coding '(utf-8 . utf-8)
                            :noquery t
                            :filter 'emms-player-mpv-ipc-filter
                            :sentinel 'emms-player-mpv-ipc-sentinel)))
                    (set-process-filter
                     (tq-process mpv--queue)
                     (lambda (_proc string)
                       (ignore-errors
                         (mpv--tq-filter mpv--queue string)))))
                  (run-hooks 'mpv-on-start-hook)
                  (run-hooks 'mpv-started-hook)
                  t)

                (defun ordenada-emms-connect-to-mpv-on-startup (data)
                  "Connect to the emms process with DATA."
                  (interactive)
                  (when (string= (alist-get 'event data) "start-file")
                    (ordenada-emms-connect-to-mpv-proc)))

                (advice-add 'mpv-kill :override 'ordenada-emms-mpv-kill)
                (add-hook 'emms-player-mpv-event-functions
                          #'ordenada-emms-connect-to-mpv-on-startup)
                (require 'emms-player-mpv)
                (setq emms-player-list '(emms-player-mpv))
                (add-to-list 'emms-player-mpv-parameters "--ytdl-format=best")
                (add-to-list 'emms-player-mpv-parameters "--force-window=no")
              ''}

              (with-eval-after-load 'emms-tag-editor
                (let ((mp3-function (assoc "mp3"
                                           emms-tag-editor-tagfile-functions)))
                  (add-to-list
                   'emms-tag-editor-tagfile-functions
                   `("aac" ,(cadr mp3-function) ,(caddr mp3-function))))
                (add-to-list 'emms-tag-editor-tagfile-functions
                             '("m4a" "${lib.getExe pkgs.atomicparsley}"
                               ((info-artist . "--artist")
                                (info-title . "--title")
                                (info-album . "--album")
                                (info-tracknumber . "--tracknum")
                                (info-year . "--year")
                                (info-genre . "--genre")
                                (info-note . "--comment")
                                (info-albumartist . "--albumArtist")
                                (info-composer . "--composer")))))

              (setq emms-playlist-buffer-name "*EMMS Playlist*")
              (setq emms-playlist-mode-center-when-go t)
              (setq emms-history-file
                    (expand-file-name "emacs/emms-history"
                                      (xdg-cache-home)))
              (setq emms-seek-seconds 15)
              (setq emms-source-file-default-directory
                    (substitute-env-vars "${xdg.userDirs.music}"))
              (setq emms-repeat-playlist t)
              (setq emms-info-functions '(emms-info-${emacs.emms.infoMethod}))
              (setq emms-mode-line-format "%s")
              (setq emms-mode-line-icon-enabled-p nil)

              (with-eval-after-load 'emms-browser
                (eval-when-compile
                  (require 'emms-browser))
                (emms-browser-make-filter
                 "all-files" (emms-browser-filter-only-type 'file))
                (emms-browser-make-filter
                 "last-week" (emms-browser-filter-only-recent 7))
                (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
                (setq emms-browser-switch-to-playlist-on-add t)
                (setq emms-browser-thumbnail-small-size 64)
                (setq emms-browser-thumbnail-medium-size 128)))
          '';
        elispPackages = with pkgs.emacsPackages; [ emms ];
      };
    };
}
