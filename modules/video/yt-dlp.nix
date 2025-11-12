{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "yt-dlp";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "yt-dlp" { };
      musicDownloadArgs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of command line arguments for music downloads.";
        default = [ ];
      };
      videoDownloadArgs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of command line arguments for video downloads.";
        default = [ ];
      };
      key = lib.mkOption {
        type = lib.types.str;
        description = "Keybinding to launch ytdl.";
        default = "y";
      };
      extraConfig = lib.mkOption {
        type = lib.types.attrs;
        description = "Extra yt-dlp configuration";
        default = { };
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.yt-dlp = with config.ordenada.features.yt-dlp; {
        inherit package;
        enable = true;
        settings = {
          output = "%(title)s [%(id)s].%(ext)s";
        } // extraConfig;
      };
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-yt-dlp";
        config =
          let
            ffmpegArgs = [
              "--ffmpeg-location"
              "${lib.getExe pkgs.ffmpeg}"
            ];
          in
          with config.ordenada.features; # elisp
          ''
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${yt-dlp.key}" #'ytdl-show-list))
            (with-eval-after-load 'ytdl
              (require 'env)
              (keymap-set ytdl--dl-list-mode-map "a" #'ytdl-download)
              (setopt ytdl-command "${lib.getExe yt-dlp.package}")
              (setopt ytdl-download-folder (substitute-env-vars "${xdg.userDirs.download}"))
              (setopt ytdl-music-folder (substitute-env-vars "${xdg.userDirs.music}"))
              (setopt ytdl-video-folder (substitute-env-vars "${xdg.userDirs.videos}"))
              (setopt ytdl-mode-line nil)
              (setopt ytdl-music-extra-args
                      ${ordenada-lib.elisp.toList (yt-dlp.musicDownloadArgs ++ ffmpegArgs)})
              (setopt ytdl-video-extra-args
                      ${ordenada-lib.elisp.toList (yt-dlp.videoDownloadArgs ++ ffmpegArgs)}))
          '';
        elispPackages = with pkgs.emacsPackages; [ ytdl ];
      };
    };
}
