{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.ordenada.features.sway;
  grimScript =
    {
      region ? false,
      clipboard ? false,
    }:
    pkgs.writeShellScriptBin "screenshot${if region then "-region" else ""}${if clipboard then "-clipboard" else ""}" ''
      ${pkgs.grim}/bin/grim ${if region then "-g \"$(${pkgs.slurp}/bin/slurp)\"" else ""} \
      ${
        if clipboard then
          "- |  ${pkgs.wl-clipboard}/bin/wl-copy"
        else
          "-t jpeg ${config.ordenada.features.xdg.userDirs.pictures}/$(date +%Y%m%d-%H%M%S)"
      }
    '';
  grimScriptRegionClipboard = grimScript {
    region = true;
    clipboard = true;
  };
  wfRecorderScript =
    {
      region ? false,
    }:
    pkgs.writeShellScriptBin "screencast${if region then "-region" else ""}" ''
      ${pkgs.wf-recorder}/bin/wf-recorder -x yuv420p \
      ${if region then "-g \"$(${pkgs.slurp}/bin/slurp)\"" else ""} \
      -f ${config.ordenada.features.xdg.userDirs.videos}/$(date +%Y%m%d-%H%M%S).mp4
    '';
  wfRecorderScriptRegion = wfRecorderScript { region = true; };
  convertToGif = pkgs.writeShellScriptBin "convert-to-gif" ''
    ${pkgs.ffmpeg}/bin/ffmpeg -i "$1" \
    -filter_complex "[0:v] palettegen" /tmp/gif_palette.gif
    ${pkgs.ffmpeg}/bin/ffmpeg -i "$1" -i /tmp/gif_palette.gif \
    -filter_complex "[0:v] fps=10,scale=720:-1 [new];[new][1:v] paletteuse" "$2"
    ${pkgs.wl-clipboard}/bin/wl-copy -t image/png < "$2"
  '';
in
{
  options = {
    ordenada.features.sway = {
      enable = mkEnableOption "the Sway feature";
      package = mkOption {
        type = types.package;
        description = "The Sway package to use.";
        default = pkgs.sway;
      };
      autoStartTty = mkOption {
        type = types.nullOr types.str;
        description = "The tty to launch Sway in.";
        default = null;
      };
      modifier = mkOption {
        type = types.str;
        description = "The modifier to bind Sway keys to.";
        default = "Mod4";
      };
      keybindings = mkOption {
        type = types.attrs;
        description = "The Sway keybindings.";
        default = { };
      };
      extraConfig = mkOption {
        type = types.attrs;
        default = { };
        description = "Extra configuration for Sway.";
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      security.polkit.enable = true;
      environment.loginShellInit = lib.mkIf (cfg.autoStartTty != null) ''
        [[ $(tty) == ${cfg.autoStartTty} ]] && exec ${cfg.package}/bin/sway
      '';
      environment.sessionVariables.NIXOS_OZONE_WL = "1";
      security.pam.services.swaylock = { };
    })
    {
      home-manager = mkHomeConfig config "sway" (user: {
        xdg.desktopEntries = {
          screenshot = {
            name = "Screenshot Region";
            exec = "${grimScriptRegionClipboard}/bin/screenshot-region-clipboard %U";
          };
          screencast = {
            name = "Screencast Region";
            exec = "${wfRecorderScriptRegion}/bin/screencast-region %U";
          };
        };
        home.packages = with pkgs; [
          wev
          wl-clipboard
          libxkbcommon
          grimScriptRegionClipboard
          wf-recorder
          wfRecorderScriptRegion
          convertToGif
        ];
        programs.swaylock =
          with user.features.theme.scheme;
          let
            transparent = "00000000";
          in
          {
            enable = true;
            package = pkgs.swaylock-effects;
            settings = {
              clock = true;
              indicator = true;
              indicator-thickness = 7;
              effect-vignette = "0.5:0.5";
              hide-keyboard-layout = true;
              image = "${user.features.theme.wallpaper}";
              color = base00;
              inside-color = base00;
              inside-clear-color = base00;
              inside-caps-lock-color = base00;
              inside-ver-color = base00;
              inside-wrong-color = base00;
              key-hl-color = base0B;
              layout-bg-color = base00;
              layout-border-color = base01;
              layout-text-color = base05;
              line-uses-inside = false;
              line-uses-ring = false;
              line-color = transparent;
              line-ver-color = transparent;
              line-clear-color = transparent;
              line-wrong-color = transparent;
              ring-color = base01;
              ring-clear-color = base0A;
              ring-caps-lock-color = base01;
              ring-ver-color = base0B;
              ring-wrong-color = base08;
              separator-color = transparent;
              text-color = base05;
              text-clear-color = base05;
              text-caps-lock-color = base05;
              text-ver-color = base05;
              text-wrong-color = base05;
              font = user.features.fontutils.fonts.sans.name;
            };
          };
        programs.swayr = {
          enable = true;
          systemd.enable = true;
        };
        wayland.windowManager.sway = {
          enable = true;
          systemd.enable = true;
          xwayland = true;
          wrapperFeatures = {
            base = true;
            gtk = true;
          };
          extraSessionCommands = ''
            export QT_QPA_PLATFORM=wayland
            export XDG_SESSION_TYPE=wayland
            export XDG_CURRENT_DESKTOP=sway
            export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
            export SDL_VIDEODRIVER=wayland
            export _JAVA_AWT_WM_NONREPARENTING=1
          '';
          config =
            with user.features.theme.scheme.withHashtag;
            {
              terminal = "alacritty";
              defaultWorkspace = "workspace number 1";
              modifier = user.features.sway.modifier;
              input = with user.features.keyboard.layout; {
                "type:keyboard" = {
                  xkb_layout = name;
                  xkb_options = lib.strings.concatStringsSep "," options;
                } // (if variant != "" then { xkb_variant = variant; } else { });
                "type:touchpad" = {
                  dwt = "enabled";
                  tap = "enabled";
                  middle_emulation = "enabled";
                };
              };
              output = {
                "*" = {
                  bg = "${user.features.theme.wallpaper} fill";
                };
              };
              seat."*" = with user.features.gtk.cursorTheme; {
                xcursor_theme = "${name} ${toString user.features.gtk.cursorSize}";
              };
              keybindings = lib.mkOptionDefault user.features.sway.keybindings;
              floating = {
                titlebar = false;
                border = 2;
                criteria = [ { app_id = "Waydroid"; } ];
              };
              colors =
                with pkgs.lib.nix-rice.color;
                let
                  background = base00;
                  focused =
                    if user.features.theme.polarity == "dark" then
                      toRgbHex (darken 50 (hexToRgba base0D))
                    else
                      toRgbHex (brighten 50 (hexToRgba base0D));
                  indicator = focused;
                  unfocused = base01;
                  text = base05;
                  urgent = base08;
                in
                {
                  inherit background;
                  urgent = {
                    inherit background indicator text;
                    border = urgent;
                    childBorder = urgent;
                  };
                  focused = {
                    inherit background indicator text;
                    border = focused;
                    childBorder = focused;
                  };
                  focusedInactive = {
                    inherit background indicator text;
                    border = unfocused;
                    childBorder = unfocused;
                  };
                  unfocused = {
                    inherit background indicator text;
                    border = unfocused;
                    childBorder = unfocused;
                  };
                  placeholder = {
                    inherit background indicator text;
                    border = unfocused;
                    childBorder = unfocused;
                  };
                };
              window = {
                titlebar = false;
                border = 2;
              };
              gaps.inner = 12;
              bars = [ ];
            }
            // cfg.extraConfig;
        };
      });
    }
  ];
}
