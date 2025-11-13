{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "sway";
  options =
    { pkgs, ... }:
    let
      inherit (lib) mkOption mkPackageOption types;
    in
    {
      package = mkPackageOption pkgs "sway" { };
      modifier = mkOption {
        type = types.str;
        description = "The modifier to bind Sway keys to.";
        default = "Mod4";
      };
      left = mkOption {
        type = types.str;
        description = "The key to use for for the left orientation.";
        default = "h";
      };
      right = mkOption {
        type = types.str;
        description = "The key to use for for the right orientation.";
        default = "l";
      };
      up = mkOption {
        type = types.str;
        description = "The key to use for for the up orientation.";
        default = "k";
      };
      down = mkOption {
        type = types.str;
        description = "The key to use for for the down orientation.";
        default = "j";
      };
      extraKeybindings = mkOption {
        type = ordenada-lib.types.fnOrAttrs;
        description = "Extra Sway keybindings.";
        default = { };
      };
      extraConfig = mkOption {
        type = types.attrs;
        default = { };
        description = "Extra Sway configuration.";
      };
    };
  globals =
    { config, ... }:
    {
      apps.wm = "${config.ordenada.features.sway.package}/bin/sway";
      wayland = true;
    };
  nixos = {
    hardware.graphics.enable = true;
    security.polkit.enable = true;
    environment.sessionVariables.NIXOS_OZONE_WL = "1";
  };
  homeManager =
    { config, lib, pkgs, ... }:
    {
      programs.swayr = {
        enable = true;
        systemd.enable = true;
      };
      wayland.windowManager.sway = with config.ordenada.features.sway; {
        inherit package;
        enable = true;
        systemd.enable = true;
        xwayland = true;
        wrapperFeatures = {
          base = true;
          gtk = true;
        };
        extraSessionCommands = # sh
          ''
            export QT_QPA_PLATFORM=wayland
            export XDG_SESSION_TYPE=wayland
            export XDG_CURRENT_DESKTOP=sway
            export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
            export SDL_VIDEODRIVER=wayland
            export _JAVA_AWT_WM_NONREPARENTING=1
          '';
        config =
          with config.ordenada.features.theme.scheme.withHashtag;
          lib.recursiveUpdate {
            inherit modifier;
            defaultWorkspace = "workspace number 1";
            input = with config.ordenada.features.keyboard.layout; {
              "type:keyboard" =
                {
                  xkb_layout = name;
                  xkb_options = lib.strings.concatStringsSep "," options;
                }
                // (lib.optionalAttrs (variant != "") {
                  xkb_variant = variant;
                });
              "type:touchpad" = {
                dwt = "enabled";
                tap = "enabled";
                middle_emulation = "enabled";
              };
            };
            output = {
              "*" = {
                bg = "${config.ordenada.features.theme.wallpaper} fill";
              };
            };
            seat."*" = with config.ordenada.features.gtk.cursorTheme; {
              xcursor_theme = "${name} ${toString config.ordenada.features.gtk.cursorSize}";
            };
            floating = {
              titlebar = false;
              border = 2;
            };
            colors =
              with ordenada-lib.nix-rice.color;
              let
                background = base00;
                focused = toRgbHex (
                  (if config.ordenada.features.theme.polarity == "dark" then darken else brighten) 50 (
                    hexToRgba base0D
                  )
                );
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
            up = up;
            down = down;
            left = left;
            right = right;
            keybindings =
              let
                apps = config.ordenada.globals.apps;
              in
              lib.mkOptionDefault (
                { }
                // lib.optionalAttrs (apps.launcher != null) {
                  "${modifier}+d" = "exec ${apps.launcher}";
                }
                // lib.optionalAttrs (apps.terminal != null) {
                  "${modifier}+Return" = "exec ${apps.terminal}";
                }
                // lib.optionalAttrs (apps.passwordManager != null) {
                  "${modifier}+p" = "exec ${apps.passwordManager}";
                }
                // ordenada-lib.mkKeybindings keybindings (
                  apps
                  // {
                    modifier = modifier;
                    right = right;
                    left = left;
                    down = down;
                    up = up;
                  }
                )
              );
          } extraConfig;
      };
      home.packages = with pkgs; [
        wl-clipboard
        wtype
      ];
    };
}
