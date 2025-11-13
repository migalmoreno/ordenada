{ lib, mkFeature, ... }:

let
  mkOpts =
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
  mkSettings =
    config: with config.ordenada.features.theme.scheme.withHashtag; {
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
mkFeature {
  name = "bemenu";
  options =
    { config, pkgs, ... }:
    let
      inherit (lib) mkOption mkPackageOption types;
      enabled = config.ordenada.features.bemenu.enable;
    in
    {
      package = mkPackageOption pkgs "bemenu" { };
      height = mkOption {
        type = types.int;
        description = "The height of the bemenu prompt.";
        default = 34;
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
    };
  globals =
    { config, pkgs, ... }:
    {
      apps.launcher =
        with config.ordenada.features.bemenu;
        lib.mkIf config.ordenada.features.bemenu.enableLauncher (
          lib.mkForce ''
            ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop --dmenu="${package}/bin/bemenu ${mkOpts (mkSettings config)}"
          ''
        );
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.bemenu = {
        enable = true;
        package = config.ordenada.features.bemenu.package;
        settings = mkSettings config;
      };
      services.gpg-agent.pinentry.package = lib.mkIf config.ordenada.features.bemenu.enablePinentry (
        lib.mkForce (
          pkgs.writeShellScriptBin "pinentry-bemenu" (
            with config.ordenada.features.bemenu;
            ''
              PATH="$PATH:${pkgs.coreutils}/bin:${package}/bin"
              unset BEMENU_OPTS
              "${pkgs.pinentry-bemenu}/bin/pinentry-bemenu" ${
                mkOpts (
                  removeAttrs (mkSettings config) [
                    "cw"
                    "hp"
                    "ch"
                  ]
                )
              }
            ''
          )
        )
      );
    };
}
