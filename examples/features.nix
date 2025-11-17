{
  ordenada.features = {
    userInfo.enable = true;
    hostInfo = {
      enable = true;
      timeZone = "UTC";
    };
    home.enable = true;
    theme.enable = true;
    xdg.enable = true;

    # shell
    bash.enable = true;

    # terminal
    alacritty.enable = true;

    # system
    keyboard.enable = true;
    networking.enable = true;
    fontutils.enable = true;

    # editor
    emacs = {
      enable = true;
      keymaps.enable = true;
      appearance.enable = true;
      ace-window.enable = true;
      all-the-icons.enable = true;
      modus-themes.enable = true;
      org = {
        enable = true;
        orgModern = true;
      };
      browse-url.enable = true;
      completion.enable = true;
      embark.enable = true;
      marginalia.enable = true;
      orderless.enable = true;
      vertico.enable = true;
      dired.enable = true;
      project.enable = true;
      apheleia.enable = true;
      flymake.enable = true;
      rainbow-delimiters.enable = true;
      xref.enable = true;
      eglot.enable = true;
      shell.enable = true;
      eshell.enable = true;
      eat.enable = true;
      vterm.enable = true;
      tramp.enable = true;
      emms.enable = true;
      help.enable = true;
      which-key.enable = true;
    };

    # browser
    firefox.enable = true;
    nyxt = {
      enable = true;
      defaultBrowser = true;
      appearance.enable = true;
      blocker.enable = true;
      emacs.enable = true;
      userscript.enable = true;
      search-engines.enable = true;
      router.enable = true;
      tailor.enable = true;
      mosaic.enable = true;
    };

    # lang
    clojure.enable = true;
    elisp.enable = true;
    lisp.enable = true;
    javascript.enable = true;
    nix.enable = true;
    json.enable = true;
    yaml.enable = true;
    markdown.enable = true;

    # development
    compile.enable = true;
    direnv.enable = true;

    # wm
    sway.enable = true;
    waybar.enable = true;
    bemenu.enable = true;
    rofi = {
      enable = true;
      enableLauncher = false;
    };

    # video
    mpv.enable = true;
    yt-dlp.enable = true;

    # torrent
    transmission.enable = true;

    # scripts
    scripts.screenshot.enable = true;
  };
}
