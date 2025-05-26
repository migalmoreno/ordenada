{ config, lib, pkgs, ... }:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.ordenada.features.sway;
  grimScript = { region ? false, clipboard ? false, }:
    pkgs.writeShellScriptBin "screenshot${if region then "-region" else ""}${
      if clipboard then "-clipboard" else ""
    }" ''
      ${pkgs.grim}/bin/grim ${
        if region then ''-g "$(${pkgs.slurp}/bin/slurp)"'' else ""
      } \
      ${if clipboard then
        "- |  ${pkgs.wl-clipboard}/bin/wl-copy"
      else
        "-t jpeg ${config.ordenada.features.xdg.userDirs.pictures}/$(date +%Y%m%d-%H%M%S)"}
    '';
  grimScriptRegionClipboard = grimScript {
    region = true;
    clipboard = true;
  };
  wfRecorderScript = { region ? false, }:
    pkgs.writeShellScriptBin "screencast${if region then "-region" else ""}" ''
      ${pkgs.wf-recorder}/bin/wf-recorder -x yuv420p \
      ${if region then ''-g "$(${pkgs.slurp}/bin/slurp)"'' else ""} \
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
in {
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
    (lib.mkIf cfg.enable (lib.mkMerge [{
      ## TODO: Use a `setGlobal` function here to check for `ordenada.globals.wm === null`
      ##       and print a warning if so
      ordenada.globals.wm = "${cfg.package}/bin/sway";
      ordenada.globals.wayland = true;

      security.polkit.enable = true;
      environment.loginShellInit = lib.mkIf (cfg.autoStartTty != null) ''
        [[ $(tty) == ${cfg.autoStartTty} ]] && exec ${cfg.package}/bin/sway
      '';
      environment.sessionVariables.NIXOS_OZONE_WL = "1";
    }
    ]))
    {
      home-manager = mkHomeConfig config "sway" (user: {
        xdg.desktopEntries = {
          screenshot = {
            name = "Screenshot Region";
            exec =
              "${grimScriptRegionClipboard}/bin/screenshot-region-clipboard %U";
          };
          screencast = {
            name = "Screencast Region";
            exec = "${wfRecorderScriptRegion}/bin/screencast-region %U";
          };
        };
        home.packages = with pkgs; [
          grimScriptRegionClipboard
          wf-recorder
          wfRecorderScriptRegion
          convertToGif
        ];
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
          config = with user.features.theme.scheme.withHashtag;
            lib.recursiveUpdate {
              defaultWorkspace = "workspace number 1";
              modifier = user.features.sway.modifier;
              input = with user.features.keyboard.layout; {
                "type:keyboard" = {
                  xkb_layout = name;
                  xkb_options = lib.strings.concatStringsSep "," options;
                } // (lib.optionalAttrs (variant != "") {
                  xkb_variant = variant;
                });
                "type:touchpad" = {
                  dwt = "enabled";
                  tap = "enabled";
                  middle_emulation = "enabled";
                };
              };
              output = {
                "*" = { bg = "${user.features.theme.wallpaper} fill"; };
              };
              seat."*" = with user.features.gtk.cursorTheme; {
                xcursor_theme =
                  "${name} ${toString user.features.gtk.cursorSize}";
              };
              keybindings = lib.mkOptionDefault user.features.sway.keybindings;
              floating = {
                titlebar = false;
                border = 2;
              };
              colors = with pkgs.lib.nix-rice.color;
                let
                  background = base00;
                  focused = toRgbHex
                    ((if user.features.theme.polarity == "dark" then
                      darken
                    else
                      brighten) 50 (hexToRgba base0D));
                  indicator = focused;
                  unfocused = base01;
                  text = base05;
                  urgent = base08;
                in {
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
            } cfg.extraConfig;
        };
      });
    }
  ];
}
