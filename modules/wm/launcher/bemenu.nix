{ config, lib, pkgs, ... }:

with pkgs.lib.ordenada;

let
  inherit (lib) mkEnableOption mkOption types;
  cfg = config.ordenada.features.bemenu;
  mkBemenuOpts = opts:
    toString (lib.mapAttrsToList (name: value:
      "--${name} ${
        if builtins.isInt value then
          (toString value)
        else if builtins.isBool value then
          ""
        else
          "'${value}'"
      }") opts);
in {
  options = {
    ordenada.features.bemenu = {
      enable = mkEnableOption "the bemenu feature";
      package = mkOption {
        type = types.package;
        default = pkgs.bemenu;
        description = "The bemenu package to use.";
      };
      height = mkOption {
        type = types.int;
        description = "The height of the bemenu prompt.";
        default = 34;
      };
    };
  };
  config = {
    ## TODO: Use a `setGlobal` function here to check for `ordenada.globals.launcher === null`
    ##       and print a warning if so
    ordenada.globals.launcher = "${cfg.package}/bin/bemenu";

    home-manager = mkHomeConfig config "bemenu" (user:
      with config.home-manager.users.${user.name}.programs.bemenu; {
        programs.bemenu = {
          enable = true;
          package = user.features.bemenu.package;
          settings = with user.features.theme.scheme.withHashtag; {
            line-height = 34;
            ignorecase = true;
            hp = 10;
            cw = 1;
            ch = 20;
            tf = base05;
            tb = base02;
            ff = base05;
            fb = base01;
            nf = base05;
            nb = base01;
            af = base05;
            ab = base01;
            cf = base05;
            cb = base01;
            hf = base01;
            hb = base0D;
            fn = with user.features.fontutils.fonts.monospace;
              "${name} ${toString size}";
          };
        };
        services.gpg-agent.pinentry.package = lib.mkForce (
          pkgs.writeShellScriptBin "pinentry-bemenu" ''
            PATH="$PATH:${pkgs.coreutils}/bin:${package}/bin"
            unset BEMENU_OPTS
            "${pkgs.pinentry-bemenu}/bin/pinentry-bemenu" ${
              mkBemenuOpts (removeAttrs settings [ "cw" "hp" "ch" ])
            }
          '');
        wayland.windowManager.sway.config.menu = ''
          ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop \
           --dmenu="${package}/bin/bemenu ${mkBemenuOpts settings}"
        '';
      });
  };
}
