{ lib, ordenada-lib, ... }:

rec {
  options.ordenada.features.pipewire = {
    enable = lib.mkEnableOption "to enable the Pipewire feature";
  };
  config.ordenada.modules = ordenada-lib.mkFeature "docker" {
    inherit options;
    nixos =
      { config, ... }:
      lib.mkIf config.ordenada.features.pipewire.enable {
        security.rtkit.enable = true;
        services.pipewire = {
          enable = true;
          pulse.enable = true;
        };
      };
    homeManager =
      { config, pkgs, ... }:
      lib.mkIf config.ordenada.features.pipewire.enable {
        home.packages = [ pkgs.pulseaudio ];
        home.sessionVariables = {
          RTC_USE_PIPEWIRE = "true";
        };
      };
  };
}
