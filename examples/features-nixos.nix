{
  ordenada.features = {
    # browser
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

    # development
    android = {
      enable = true;
      allowUnfreeAndAcceptLicenses = true;
      activeSdkVersion = "35";
      sdks = [
        { platformVersion = "35"; }
        { platformVersion = "36"; }
      ];
      emulators = [
        {
          name = "Small Phone";
          platformVersion = "35";
          persistentData = true;
        }
        {
          platformVersion = "36";
        }
      ];
    };

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

    # scripts
    scripts.screenshot.enable = true;
  };

}
