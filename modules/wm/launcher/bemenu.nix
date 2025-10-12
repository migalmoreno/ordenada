{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkOption
    mkPackageOption
    types
    ;
  inherit (pkgs.lib.ordenada) mkHomeConfig;
  cfg = config.ordenada.features.bemenu;
  mkBemenuOpts =
    opts:
    toString (
      lib.mapAttrsToList (
        name: value:
        "--${name} ${
          if builtins.isInt value then
            (toString value)
          else if builtins.isBool value then
            ""
          else
            "'${value}'"
        }"
      ) opts
    );
  menuSettings = with config.ordenada.features.theme.scheme.withHashtag; {
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
    fn = with config.ordenada.features.fontutils.fonts.monospace; "${name} ${toString size}";
  };
in
{
  options.ordenada.features.bemenu = {
    enable = mkEnableOption "the bemenu feature";
    package = mkPackageOption pkgs "bemenu" { };
    height = mkOption {
      type = types.int;
      description = "The height of the bemenu prompt.";
      default = 34;
    };
    enableLauncher = mkOption {
      type = types.bool;
      description = "Whether to enable this feature as the global launcher.";
      default = true;
    };
    enablePinentry = mkOption {
      type = types.bool;
      description = "Whether to enable this feature as the global pinentry.";
      default = true;
    };
  };
  config = lib.mkIf cfg.enable {
    ordenada.globals.launcher = ''
      ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop \
        --dmenu="${cfg.package}/bin/bemenu ${mkBemenuOpts menuSettings}"
    '';
    home-manager = mkHomeConfig config "bemenu" (
      user: with config.home-manager.users.${user.name}.programs.bemenu; {
        programs.bemenu = {
          enable = true;
          package = user.features.bemenu.package;
          settings = menuSettings;
        };
        services.gpg-agent.pinentry.package = lib.mkIf cfg.enablePinentry (
          lib.mkForce (
            pkgs.writeShellScriptBin "pinentry-bemenu" ''
              PATH="$PATH:${pkgs.coreutils}/bin:${package}/bin"
              unset BEMENU_OPTS
              "${pkgs.pinentry-bemenu}/bin/pinentry-bemenu" ${
                mkBemenuOpts (
                  removeAttrs settings [
                    "cw"
                    "hp"
                    "ch"
                  ]
                )
              }
            ''
          )
        );
      }
    );
  };
}
