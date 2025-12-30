{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  inherit (lib)
    mkOption
    mkPackageOption
    types
    ;
in
mkFeature {
  name = "aerospace";
  options =
    { config, pkgs, ... }:
    with config.ordenada.globals;
    with config.ordenada.features.xdg;
    {
      modifier = mkOption {
        type = types.str;
        description = "The modifier to bind aerospace keys to.";
        default = "cmd";
      };
      left = mkOption {
        type = types.str;
        description = "The key to use for for the left orientation.";
        default = "left";
      };
      right = mkOption {
        type = types.str;
        description = "The key to use for for the right orientation.";
        default = "right";
      };
      up = mkOption {
        type = types.str;
        description = "The key to use for for the up orientation.";
        default = "up";
      };
      down = mkOption {
        type = types.str;
        description = "The key to use for for the down orientation.";
        default = "down";
      };
      extraKeybindings = mkOption {
        type = ordenada-lib.types.fnOrAttrs;
        apply =
          x:
          ordenada-lib.getFnOrAttrsValue x (
            apps
            // {
              modifier = modifier;
              right = right;
              left = left;
              down = down;
              up = up;
            }
          );
        description = "Extra Aerospace keybindings.";
        default = { };
      };
      extraConfig = mkOption {
        type = types.attrs;
        default = { };
        description = "Extra Aerospace configuration.";
      };
    };
  darwin =
    { config, pkgs, ... }:
    {
      options.ordenada = {
        features.aerospace.package = mkPackageOption pkgs "aerospace" { };
        globals.apps.wm = "${config.ordenada.features.aerospace.package}/Applictions/AeroSpace.app";
      };
    };
  homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    with config.ordenada.features.aerospace;
    {
      programs.aerospace = {
        enable = true;
        package = package;

        launchd = {
          enable = true;
          keepAlive = true;
        };

        settings = lib.recursiveUpdate {
          config-version = 2;
          enable-normalization-flatten-containers = true;
          enable-normalization-opposite-orientation-for-nested-containers = true;
          default-root-container-layout = "tiles";
          default-root-container-orientation = "auto";

          on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];
          automatically-unhide-macos-hidden-apps = true;

          gaps = {
            inner = {
              horizontal = 12;
              vertical = 12;
            };
            outer = {
              left = 12;
              right = 12;
              top = 12;
              bottom = 12;
            };
          };

          persistent-workspaces = [
            "1"
            "2"
            "3"
            "4"
            "5"
            "6"
            "7"
            "8"
            "9"
            "0"
          ];

          mode.main.binding =
            with config.ordenada.globals.apps;
            lib.mkOptionDefault (
              { }
              // lib.optionalAttrs (terminal != null) {
                "${modifier}-enter" = "exec-and-forget ${terminal}";
              }
              // lib.optionalAttrs (launcher != null) {
                "${modifier}-shift-d" = "exec-and-forget ${launcher}";
              }
              // lib.optionalAttrs (passwordManager != null) {
                "${modifier}-shift-p" = "exec-and-forget ${passwordManager}";
              }
              // {
                "${modifier}-shift-q" = "close";

                "${modifier}-${left}" = "focus left";
                "${modifier}-${down}" = "focus down";
                "${modifier}-${up}" = "focus up";
                "${modifier}-${right}" = "focus right";

                "${modifier}-shift-${left}" = "move left";
                "${modifier}-shift-${down}" = "move down";
                "${modifier}-shift-${up}" = "move up";
                "${modifier}-shift-${right}" = "move right";

                "${modifier}-shift-l" = "join-with right";
                "${modifier}-shift-h" = "join-with left";
                "${modifier}-shift-j" = "join-with down";
                "${modifier}-shift-k" = "join-with up";
                "${modifier}-shift-f" = "fullscreen";

                "${modifier}-s" = "layout tiles horizontal vertical";
                "${modifier}-w" = "layout accordion horizontal vertical";
                ## TODO: Rebind to mod-space once we can remap cmd to ctrl
                "${modifier}-shift-space" = "layout tiling floating";

                "${modifier}-r" = "mode resize";

                "${modifier}-1" = "workspace 1";
                "${modifier}-2" = "workspace 2";
                "${modifier}-3" = "workspace 3";
                "${modifier}-4" = "workspace 4";
                "${modifier}-5" = "workspace 5";
                "${modifier}-6" = "workspace 6";
                "${modifier}-7" = "workspace 7";
                "${modifier}-8" = "workspace 8";
                "${modifier}-9" = "workspace 9";
                "${modifier}-0" = "workspace 10";

                "${modifier}-shift-1" = "move-node-to-workspace 1";
                "${modifier}-shift-2" = "move-node-to-workspace 2";
                "${modifier}-shift-3" = "move-node-to-workspace 3";
                "${modifier}-shift-4" = "move-node-to-workspace 4";
                "${modifier}-shift-5" = "move-node-to-workspace 5";
                "${modifier}-shift-6" = "move-node-to-workspace 6";
                "${modifier}-shift-7" = "move-node-to-workspace 7";
                "${modifier}-shift-8" = "move-node-to-workspace 8";
                "${modifier}-shift-9" = "move-node-to-workspace 9";
                "${modifier}-shift-0" = "move-node-to-workspace 0";

                "${modifier}-shift-c" = "reload-config";
              }
              // extraKeybindings
            );

          mode.resize.binding = {
            "${left}" = "resize width -50";
            "${right}" = "resize width +50";
            "${up}" = "resize height -50";
            "${down}" = "resize height +50";
            "equal" = "balance-sizes";
            "esc" = "mode main";
          };
        } extraConfig;
      };

    };
}
