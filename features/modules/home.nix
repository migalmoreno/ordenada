{
  inputs,
  lib,
  mkFeature,
  ...
}:

let
  inherit (lib) mkOption types;
in
mkFeature {
  name = "home";
  options = {
    autoStartWmOnTty = mkOption {
      type = types.nullOr types.str;
      description = "The tty to launch the WM in.";
      default = null;
    };
  };
  nixos =
    { config, ... }:
    {
      imports = [
        inputs.home-manager.nixosModules.home-manager
      ];
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.backupFileExtension = "backup";
      environment.loginShellInit =
        with config.ordenada.features.home;
        (lib.mkIf (autoStartWmOnTty != null) ''
          [[ $(tty) == ${autoStartWmOnTty} ]] && exec ${config.ordenada.globals.wm}
        '');
    };
  homeManager =
    { config, ... }:
    {
      programs.home-manager.enable = true;
      targets.genericLinux.enable = true;
      home.file.".profile".text = lib.mkIf (config.ordenada.globals.shell == null) ''
        . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
      '';
    };
}
