{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "mpv";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "mpv" { };
      extraConfig = lib.mkOption {
        type = lib.types.attrs;
        description = "Extra mpv configuration.";
        default = { };
      };
      extraBindings = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        description = "Extra mpv bindings.";
        default = { };
      };
      key = lib.mkOption {
        type = lib.types.str;
        description = "Keybinding for mpv map operations.";
        default = "m";
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      xdg.mimeApps.defaultApplications = lib.genAttrs [
        "video/mp4"
        "video/mkv"
        "video/webm"
        "audio/mpeg"
      ] (lib.const "mpv.desktop");
      programs.mpv = with config.ordenada.features.mpv; {
        inherit package;
        enable = true;
        config =
          {
            border = "no";
            volume = 100;
            screenshot-directory = "${config.ordenada.features.xdg.baseDirs.dataHome}/mpv/screenshots";
            autofit = "800x800";
            osd-border-size = 2;
            osd-bar = "yes";
            osd-level = 0;
            slang = "en";
            ytdl-raw-options = "ignore-config=,sub-lang=en,write-auto-sub=";
            "script-opts-add=osc-visibility" = "never";
            "script-opts-add=osc-windowcontrols" = "no";
            "save-position-on-quit" = true;
          }
          // lib.genAttrs [ "osd-font" "sub-font" ] (
            lib.const "${config.ordenada.features.fontutils.fonts.sans.name}"
          )
          // extraConfig;
        bindings =
          let
            runWithEmacs = cmd: "run \"/bin/sh\" \"-c\" \"emacsclient -e '${cmd}'\"";
          in
          {
            "ctrl+a" = "seek 0 absolute-percent";
            "ctrl+e" = "seek 100 absolute-percent";
            "ctrl+f" = "seek 5 relative";
            "ctrl+b" = "seek -5 relative";
            "Shift+n" = "add chapter 1";
            "Shift+p" = "add chapter -1";
            "F" = "cycle fullscreen";
            "D" = runWithEmacs "(ordenada-mpv-download)";
            "Alt+c" = runWithEmacs "(ordenada-mpv-capture)";
            "M" = "cycle mute";
            "+" = "add volume 2";
            "-" = "add volume -2";
            ":" = "script-binding console/enable";
            "s" = "screenshot video";
            "Q" = "quit-watch-later";
            "O" = "no-osd cycle-values osd-level 3 0";
            "o" = "osd-bar show-progress";
            "v" = "cycle sub-visibility";
            "b" = "cycle sub";
            "n" = "script-message osc-visibility always";
            "N" = "script-message osc-visibility never";
            "L" = "cycle-values loop-file \"inf\" \"no\"";
          }
          // extraBindings;
        scripts = with pkgs.mpvScripts; [ mpris ];
      };
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-mpv";
        config =
          with config.ordenada.features; # elisp
          ''
            (eval-when-compile
              (require 'mpv))

            (defvar ordenada-mpv-map nil
              "Map to bind `mpv' commands under.")
            (define-prefix-command 'ordenada-mpv-map)

            ${lib.optionalString yt-dlp.enable ''
              (eval-when-compile
                (require 'ytdl)
                (require 'cl-lib))

              (cl-defun ordenada-mpv-play-url (url &optional format
                                                   &key
                                                   audio repeat
                                                   (formats t) (select t)
                                                   playlist)
                "Play URL with `mpv-start'.
                You can specify whether to PLAY the file as AUDIO, if you want to be
                prompted for FORMATS or use FORMAT, to REPEAT the file, manually SELECT what to
                do with the file, and whether to add the file to the current PLAYLIST."
                (interactive "sURI: ")
                (require 'mpv)
                (let* ((sel-format
                        (or format (and formats
                                        (ytdl-select-format url))))
                       (extra-args
                        (split-string
                         (concat
                          (format "--ytdl-format=%s"
                                  (or sel-format "best"))
                          (and audio " --video=no")
                          (and repeat " --loop-file=inf")))))
                  (if (and select (mpv-get-property "playlist"))
                      (pcase (completing-read "Play or Enqueue: "
                                              '("Play" "Enqueue"))
                        ("Play" (apply 'mpv-start url extra-args))
                        ("Enqueue" (apply 'mpv-playlist-append-url url
                                          extra-args)))
                    (if (and playlist (mpv-get-property "playlist"))
                        (apply 'mpv-playlist-append-url url extra-args)
                      (apply 'mpv-start url extra-args)))))

              (defun ordenada-mpv-download ()
                "Download current mpv playback via `ytdl'."
                (interactive)
                (require 'ytdl)
                (if-let* ((dl-type (ytdl--get-download-type))
                          (track (mpv-get-property "path"))
                          (title (mpv-get-property "media-title")))
                    (ytdl--download-async
                     track
                     (expand-file-name title
                                       (ytdl--eval-field (nth 1 dl-type)))
                     (ytdl--eval-list (ytdl--eval-field (nth 2 dl-type)))
                     'ignore
                     (car dl-type))
                  (error "Mpv is not currently active")))

              (let ((map ordenada-mpv-map))
                (keymap-set map "RET" #'ordenada-mpv-play-url)
                (keymap-set map "s" #'ordenada-mpv-download))

              ${lib.optionalString emacs.embark.enable ''
                (with-eval-after-load 'embark
                  (keymap-set embark-url-map "v" #'ordenada-mpv-play-url))
              ''}
            ''}

            (defun ordenada-mpv-seek-start ()
              "Seek to the start of the current MPV stream."
              (interactive)
              (mpv-seek 0))

            (defun ordenada-mpv-playlist-shuffle ()
              "Toggle the shuffle state for the current playlist."
              (interactive)
              (mpv-run-command "playlist-shuffle"))

            (defun ordenada-mpv-kill-path ()
              "Copy the path of the current mpv stream to the clibpoard."
              (interactive)
              (when-let* ((title (mpv-get-property "media-title"))
                          (path (mpv-get-property "path")))
                (kill-new path)
                (message (format "Copied \"%s\" to the system clipboard"
                                 title))
                path))

            ${lib.optionalString emacs.embark.enable ''
              (with-eval-after-load 'embark
                (defvar ordenada-mpv-chapter-embark-actions
                  (let ((map (make-sparse-keymap)))
                    (keymap-set map "r" #'mpv-set-chapter-ab-loop)
                    map))

                (defvar ordenada-mpv-file-embark-actions
                  (let ((map (make-sparse-keymap)))
                    (keymap-set map "d" #'mpv-remove-playlist-entry)
                    map))

                (add-to-list 'embark-keymap-alist
                             '(mpv-chapter . #'ordenada-mpv-chapter-embark-actions))
                (add-to-list 'embark-keymap-alist
                             '(mpv-file . #'ordenada-mpv-file-embark-actions)))
            ''}

            (with-eval-after-load 'mpv
              (setopt mpv-seek-step 3))

            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${mpv.key}" 'ordenada-mpv-map))
            (let ((map ordenada-mpv-map))
              (keymap-set map "a" #'ordenada-mpv-seek-start)
              (keymap-set map "w" #'ordenada-mpv-kill-path)
              (keymap-set map "c" #'mpv-jump-to-chapter)
              (keymap-set map "l" #'mpv-jump-to-playlist-entry)
              (keymap-set map "n" #'mpv-playlist-next)
              (keymap-set map "p" #'mpv-playlist-prev)
              (keymap-set map "N" #'mpv-chapter-next)
              (keymap-set map "P" #'mpv-chapter-prev)
              (keymap-set map "f" #'mpv-seek-forward)
              (keymap-set map "b" #'mpv-seek-backward)
              (keymap-set map "q" #'mpv-quit)
              (keymap-set map "R" #'mpv-set-ab-loop)
              (keymap-set map "SPC" #'mpv-pause)
              (keymap-set map "r" #'mpv-toggle-loop)
              (keymap-set map "v" #'mpv-toggle-video)
              (put 'mpv-seek-forward 'repeat-map 'ordenada-mpv-map)
              (put 'mpv-seek-backward 'repeat-map 'ordenada-mpv-map)
              (put 'mpv-pause 'repeat-map 'ordenada-mpv-map))
          '';
        elispPackages = with pkgs.emacsPackages; [ mpv ];
      };
    };
}
