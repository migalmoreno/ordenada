{ lib, ordenada-lib, ... }:

rec {
  options.ordenada.features.pipewire = {
    enable = lib.mkEnableOption "to enable the Pipewire feature";
  };
  config.ordenada.modules = ordenada-lib.mkFeature "pipewire" {
    inherit options;
    nixos = {
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        pulse.enable = true;
      };
    };
    homeManager =
      { pkgs, ... }:
      {
        home.packages = [ pkgs.pulseaudio ];
        home.sessionVariables = {
          RTC_USE_PIPEWIRE = "true";
        };
      };
  };
}
