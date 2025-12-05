{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "aerospace";
  options =
    { config, pkgs, ... }:
    with config.ordenada.globals;
    with config.ordenada.features.xdg;
    let
      inherit (lib) mkOption mkPackageOption types;
    in
    {
      package = mkPackageOption pkgs "aerospace" { };
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
  globals =
    { config, ... }:
    {
      apps.wm = "${config.ordenada.features.sway.package}/bin/sway";
      wayland = true;
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
      };
userSettings =
          lib.recursiveUpdate {
            enable-normalization-flatten-containers = true;
            enable-normalization-opposite-orientation-for-nested-containers = true;
            default-root-container-layout = "tiles";
            default-root-container-orientation = "auto";

            on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];
            automatically-unhide-macos-hidden-apps = true;

            key-mapping = {
              preset = "qwerty";
            };

            gaps = {
              inner.horizontal = 8;
              inner.vertical = 8;
              outer.left = 8;
              outer.bottom = 8;
              outer.top = 8;
              outer.right = 8;
            };

            mode.main.binding = {
              "${modifier}-enter" = mkIf (
                config.ordenada.globals.terminal != null
              ) "exec-and-forget ${config.ordenada.globals.terminal}";

              "${modifier}-d" = mkIf (
                config.ordenada.globals.launcher != null
              ) "exec-and-forget ${config.ordenada.globals.launcher}";

              "${modifier}-p" = mkIf (
                config.ordenada.globals.passwordManager != null
              ) "exec-and-forget ${config.ordenada.globals.passwordManager}";

              "${modifier}-shift-q" = "close";

              "${modifier}-${left}" = "focus left";
              "${modifier}-${down}" = "focus down";
              "${modifier}-${up}" = "focus up";
              "${modifier}-${right}" = "focus right";

              "${modifier}-shift-${left}" = "move left";
              "${modifier}-shift-${down}" = "move down";
              "${modifier}-shift-${up}" = "move up";
              "${modifier}-shift-${right}" = "move right";

              # "${modifier}+b" = "splith";
              # "${modifier}+v" = "splitv";
              "${modifier}-f" = "fullscreen";
              # "${modifier}+a" = "focus parent";

              "${modifier}-s" = "layout tiles horizontal vertical";
              "${modifier}-w" = "layout accordion horizontal vertical";
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
            };

            mode.resize.binding = {
              "${left}" = "resize width -50";
              "${right}" = "resize width +50";
              "${up}" = "resize height -50";
              "${down}" = "resize height +50";
              "equal" = "balance-sizes";
              "esc" = "mode main";
            };
          }
          // extraConfig;

    };
}
