{ mkFeature, ... }:

mkFeature {
  name = "pipewire";
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
}
