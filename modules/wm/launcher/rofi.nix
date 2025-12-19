{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  inherit (ordenada-lib) mkLiteral;
  mkTheme = config: {
    "*" = with config.ordenada.features.theme.scheme.withHashtag; {
      border = 0;
      margin = 0;
      padding = 0;
      spacing = 0;

      bg0 = mkLiteral base01;
      bg1 = mkLiteral base02;

      fg0 = mkLiteral base05;
      fg1 = mkLiteral base06;

      urgent-color = mkLiteral base12;

      background-color = mkLiteral "transparent";
      text-color = mkLiteral "@fg0";
    };
    window = {
      location = mkLiteral "center";
      width = 768;
      height = 432;
      background-color = mkLiteral "@bg0";
      border = mkLiteral "2px";
      border-color = mkLiteral "@bg1";
      padding = mkLiteral "4px";
    };
    inputbar = {
      spacing = mkLiteral "8px";
      padding = mkLiteral "8px";
      background-color = mkLiteral "@bg1";
    };
    "prompt, entry, element-icon, element-text" = {
      vertical-align = mkLiteral "0.5";
    };
    prompt = {
      text-color = mkLiteral "@text-color";
    };
    textbox = {
      padding = mkLiteral "8px";
      background-color = mkLiteral "@bg1";
    };
    listview = {
      padding = mkLiteral "4px 0";
      lines = 8;
      columns = 1;
      fixed-height = true;
    };
    element = {
      padding = mkLiteral "8px";
      spacing = mkLiteral "8px";
    };
    "element normal normal" = {
      text-color = mkLiteral "@fg0";
    };
    "element normal urgent" = {
      text-color = mkLiteral "@urgent-color";
    };
    "element normal active" = {
      text-color = mkLiteral "@fg0";
    };
    "element alternate active" = {
      text-color = mkLiteral "@fg0";
    };
    "element selected" = {
      text-color = mkLiteral "@fg0";
    };
    "element selected normal, element selected active" = {
      background-color = mkLiteral "@bg1";
    };
    "element selected urgent" = {
      background-color = mkLiteral "@urgent-color";
    };
    "element-icon" = {
      size = mkLiteral "0.8em";
    };
    "element-text" = {
      text-color = mkLiteral "inherit";
    };
  };
  mkSettings =
    config:
    with config.ordenada.features.theme.scheme.withHashtag;
    with config.ordenada.features.rofi;
    {
      modi = "run,ssh,drun";
      drun-show-actions = showActions;
      show-icons = showIcons;

      font = with config.ordenada.features.fontutils.fonts.monospace; "${name} ${toString size}";

      kb-element-next = "Alt+n";
      kb-element-prev = "Alt+p";

      kb-page-next = "Super+n";
      kb-page-prev = "Super+p";

      kb-row-select = "Tab,Control+i";
      kb-secondary-paste = "Control+y";
      kb-remove-word-forward = "Alt+d";
      kb-remove-word-back = "Control+w,Control+BackSpace";
      kb-clear-line = "Control+slash";
    };
in
mkFeature {
  name = "rofi";
  options =
    { config, pkgs, ... }:
    let
      inherit (lib) mkOption mkPackageOption types;
      enabled = config.ordenada.features.rofi.enable;
    in
    {
      package = mkPackageOption pkgs "rofi" { };
      showActions = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to show actions.";
      };
      showIcons = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to show icons.";
      };
      enableLauncher = mkOption {
        type = types.bool;
        description = "Whether to enable this feature as the global launcher.";
        default = enabled;
      };
      enablePinentry = mkOption {
        type = types.bool;
        description = "Whether to enable this feature as the global pinentry.";
        default = enabled;
      };
      enablePasswordManager = mkOption {
        type = types.bool;
        description = "Whether to enable this feature as the global password manager.";
        default = enabled;
      };
    };
  globals =
    { config, pkgs, ... }:
    let
      rofiPassPkg =
        if (config.ordenada.globals.wayland == null) then pkgs.rofi-pass else pkgs.rofi-pass-wayland;
    in
    {
      apps.launcher =
        with config.ordenada.features.rofi;
        lib.mkIf enableLauncher (lib.mkForce "${package}/bin/rofi -show drun");
      apps.passwordManager =
        with config.ordenada.features.rofi;
        lib.mkIf enablePasswordManager (
          let
            rofiPassExec =
              with config.ordenada.features;
              pkgs.writeShellScriptBin "rofi-pass" ''
                export PATH="$PATH:${lib.makeBinPath [ pkgs.qrencode ]}"
                export PASSWORD_STORE_DIR="${password-store.storeDir}"
                export GNUPGHOME="${gnupg.storeDir}"
                exec "${rofiPassPkg}/bin/rofi-pass" "$@"
              '';
          in
          with config.ordenada.features.rofi;
          lib.mkIf enablePasswordManager "${rofiPassExec}/bin/rofi-pass"
        );

    };
  homeManager =
    { config, pkgs, ... }:
    lib.mkMerge [
      {
        programs.rofi = {
          enable = true;
          package = config.ordenada.features.rofi.package;
          extraConfig = mkSettings config;
          theme = mkTheme config;
        };
      }
      (lib.mkIf config.ordenada.features.rofi.enablePinentry {
        services.gpg-agent.pinentry.package =
          with config.ordenada.features.rofi;
          lib.mkIf enablePinentry (lib.mkForce pkgs.pinentry-rofi);
      })
      (lib.mkIf config.ordenada.features.rofi.enablePasswordManager {
        xdg.configFile."rofi-pass/config".text =
          let
            backend = if config.ordenada.globals.wayland then "wtype" else "xdotool";
            clipboardBackend = if config.ordenada.globals.wayland then "wl-clipboard" else "xclip";
            imageViewer =
              if (config.ordenada.globals.wayland == null) then
                "${pkgs.feh}/bin/feh"
              else
                "${pkgs.swayimg}/bin/swayimg";
          in # shell
          ''
            _image_viewer () {
              ${imageViewer} -
            }
            _pwgen () {
              ${pkgs.pwgen}/bin/pwgen -y "$@"
            }
            _rofi () {
              ${config.ordenada.globals.apps.launcher} -i -no-auto-select "$@"
            }

            default_do='copyMenu'

            autotype="Alt+1"
            type_user="Alt+2"
            type_pass="Alt+3"
            open_url="Alt+4"
            copy_name="Alt+u"
            copy_url="Alt+l"
            copy_pass="Alt+y"
            insert_pass="Alt+Y"
            show="Alt+o"
            copy_entry="Alt+2"
            type_entry="Alt+1"
            copy_menu="Alt+c"
            action_menu="Alt+a"
            type_menu="Alt+t"
            help="Alt+h"
            switch="Alt+x"

            clip=both
            backend=${backend}
            clipboard_backend=${clipboardBackend}
          '';
      })
    ];
}
