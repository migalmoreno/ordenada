{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.ordenada.features.waybar;
  waybarModule = lib.types.submodule {
    options = {
      name = mkOption {
        description = "Name of the module.";
        type = types.str;
      };
      barId = mkOption {
        description = "ID of the bar to which this module will be added to.";
        type = types.str;
        default = "primary";
      };
      style = mkOption {
        description = "CSS styles applied to this Waybar module.";
        type = types.lines;
        default = '''';
      };
      config = mkOption {
        description = "Configuration for this Waybar module.";
        type = types.attrs;
        default = { };
      };
      placement = mkOption {
        description = "Placement of the Waybar module in the bar.";
        type = types.enum [
          "modules-left"
          "modules-center"
          "modules-right"
        ];
        default = "modules-right";
      };
    };
  };
  defaultWaybarModules = {
    battery = {
      name = "battery";
      config = {
        format = "{capacity}% {icon}";
        states = {
          empty = 10;
          low = 20;
          half = 50;
          high = 80;
          full = 100;
        };
        format-icons = {
          empty = "";
          low = "";
          half = "";
          high = "";
          full = "";
        };
      };
    };
    pulseaudio = lib.mkIf config.ordenada.features.pipewire.enable {
      name = "pulseaudio";
      config = {
        format = "{volume}% {icon}";
        format-muted = "";
        format-icons = {
          default = [
            ""
            ""
            ""
          ];
        };
      };
    };
    swayLanguage = {
      name = "sway/language";
      config = {
        format = "{short}";
        on-click = "swaymsg input type:keyboard xkb_switch_layout next";
      };
    };
    clock = {
      name = "clock";
      config = with config.ordenada.features.theme.scheme.withHashtag; {
        format = "{:%a %d %b %H:%M}";
        format-alt = "{:%a %d %b (w.%V) %H:%M}";
        tooltip-format = "<tt><small>{calendar}</small></tt>";
        calendar = {
          mode-mon-col = 3;
          weeks-pos = "right";
          format = {
            weeks = "<span color='${base04}'><b>W{}</b></span>";
            today = "<span color='${base0D}'><b>{}</b></span>";
          };
        };
        actions = {
          on-click-right = "mode";
          on-scroll-up = "shift_down";
          on-scroll-down = "shift_up";
        };
      };
    };
    swayWorkspaces = {
      name = "sway/workspaces";
      placement = "modules-left";
      config = {
        disable-scroll = true;
        all-outputs = false;
        persistent-workspaces = {
          "1" = [ ];
          "2" = [ ];
          "3" = [ ];
          "4" = [ ];
          "5" = [ ];
        };
      };
      style = with config.ordenada.features.theme.scheme.withHashtag; ''
        #workspaces button {
          background: ${base02};
          color: ${base05};
          font-weight: normal;
          border: none;
          border-radius: 0.2em;
          margin: 0.3em 0.2em;
          padding: 0.3em 0.4em;
        }

        #workspaces button.active {
          background: ${base02};
        }

        #workspaces button.persistent {
          background: none;
        }

        #workspaces button.focused {
          background: ${base0D};
          color: ${base01};
        }

        #workspaces button.urgent {
          background: ${base08};
          color: ${base07};
        }
      '';
    };
    swayMode = {
      name = "sway/mode";
      placement = "modules-left";
    };
    swayWindow = {
      name = "sway/window";
      placement = "modules-center";
      config = {
        max-length = 50;
      };
    };
    swaync =
      let
        swaync = "${pkgs.swaynotificationcenter}/bin/swaync-client";
      in
      {
        name = "custom/swaync";
        config = {
          tooltip = false;
          format = "{icon}";
          format-icons = {
            none = "";
            notification = "";
            inhibited-none = "";
            inhibited-notification = "";
            dnd-none = "";
            dnd-notification = "";
            dnd-inhibited-none = "";
            dnd-inhibited-notification = "";
          };
          return-type = "json";
          exec = "${swaync} -swb";
          on-click = "${swaync} -t -sw";
          on-click-right = "${swaync} -d -sw";
          on-click-middle = "${swaync} -C -sw";
          escape = true;
        };
      };
  };
  waybarModules = with defaultWaybarModules; [
    swayWorkspaces
    swayMode
    swayWindow
    battery
    pulseaudio
    swayLanguage
    clock
    swaync
  ];
in
{
  options = {
    ordenada.features.waybar = {
      enable = mkEnableOption "the Waybar feature";
      package = mkOption {
        type = types.package;
        default = pkgs.waybar;
        description = "The waybar package to use.";
      };
      defaultModules = mkOption {
        type = types.attrsOf waybarModule;
        default = defaultWaybarModules;
        description = "Attrset of pre-built Waybar modules.";
      };
      modules = mkOption {
        type = types.listOf waybarModule;
        default = waybarModules;
        description = "The list of modules to add to Waybar.";
      };
      height = mkOption {
        type = types.int;
        default = 30;
        description = "The height of the Waybar bar.";
      };
      extraSettings = mkOption {
        type = types.attrs;
        default = { };
        description = "Extra settings for Waybar configuration.";
      };
      output = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "The list of outputs Waybar should be displayed in.";
      };
    };
  };
  config = {
    ## TODO: Use a `setGlobal` function here to check for `ordenada.globals.bar === null`
    ##       and print a warning if so
    ordenada.globals.bar = "${cfg.package}/bin/waybar";

    home-manager = mkHomeConfig config "waybar" (
      user:
      lib.mkMerge [
        {
          programs.waybar = with user.features.waybar; {
            enable = true;
            systemd.enable = true;
            package = user.features.waybar.package;
            settings.primary =
              {
                inherit height;
                layer = "top";
                position = "top";
              }
              // (lib.optionalAttrs (output != [ ]) { inherit output; })
              // extraSettings;
            style = with user.features.theme.scheme.withHashtag; ''
              * {
                font-family: ${user.features.fontutils.fonts.monospace.name}, FontAwesome;
                font-size: 14px;
                box-shadow: none;
                text-shadow: none;
                min-height: 0;
                margin: 0;
                padding: 0;
              }

              tooltip {
                opacity: 1;
                background: ${base01};
                border: 1px solid ${base02};
              }

              tooltip label {
                color: ${base05};
                padding: 0;
              }

              #waybar {
                color: ${base05};
                background: ${base01};
                border: none;
                margin: 0;
                padding: 0;
              }

              .modules-right label {
                margin: 0.3em 0.2em;
                padding: 0.3em 0.6em;
                background: ${base02};
                border-radius: 0.2em;
              }

              .modules-left {
                margin-left: 0.2em;
              }

              .modules-right {
                margin-right: 0.2em;
              }
            '';
          };
        }
        (lib.mkMerge (
          map (module: {
            programs.waybar = {
              settings.${module.barId} = {
                ${module.placement} = [ module.name ];
                ${module.name} = module.config;
              };
              style = module.style;
            };
          }) user.features.waybar.modules
        ))
      ]
    );
  };
}
