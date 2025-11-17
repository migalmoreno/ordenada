{
  inputs,
  lib,
  mkFeature,
  ...
}:

mkFeature {
  name = "home";
  options =
    { config, ... }:
    {
      autoStartWmOnTty = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "The tty to launch the WM in.";
        default = null;
      };
      applyFeaturesToAll = lib.mkOption {
        default = config.ordenada.features.home.enable;
        example = true;
        description = "Whether to apply all host features to all Home Manager configurations.";
        type = lib.types.bool;
      };
    };
  nixos =
    { config, ... }:
    {
      imports = [ inputs.home-manager.nixosModules.home-manager ];
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.backupFileExtension = "backup";
      home-manager.sharedModules = lib.mkMerge [
        [
          (lib.mkIf config.ordenada.features.home.applyFeaturesToAll {
            ordenada.features = config.ordenada.features;
          })
          {
            targets.genericLinux.enable = true;
          }
        ]
      ];
      environment.loginShellInit =
        with config.ordenada.features.home;
        (lib.mkIf (autoStartWmOnTty != null) ''
          [[ $(tty) == ${autoStartWmOnTty} ]] && exec ${config.ordenada.globals.apps.wm}
        '');
      i18n.defaultLocale = config.ordenada.features.userInfo.locale;
    };
  homeManager =
    { config, ... }:
    {
      programs.home-manager.enable = true;
      home.file.".profile" = lib.mkIf (config.ordenada.globals.apps.shell == null) {
        text = ''
          . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
        '';
      };
    };
}
