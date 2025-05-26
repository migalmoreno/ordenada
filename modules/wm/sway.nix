{ config, lib, pkgs, ... }:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.ordenada.features.sway;
in {
  options = {
    ordenada.features.sway = {
      enable = mkEnableOption "the Sway feature";
      package = mkOption {
        type = types.package;
        description = "The Sway package to use.";
        default = pkgs.sway;
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
      environment.sessionVariables.NIXOS_OZONE_WL = "1";
    }
    ]))
    {
      home-manager = mkHomeConfig config "sway" (user: {
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
