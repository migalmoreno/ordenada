{ lib, mkFeature, ... }:

mkFeature {
  name = "kanshi";
  options = with lib; {
    settings = mkOption {
      type = types.listOf types.attrs;
      description = "The list of profile settings to apply to Kanshi.";
      default = [ ];
    };
  };
  homeManager =
    { config, ... }:
    {
      services.kanshi = {
        enable = true;
        settings = config.ordenada.features.kanshi.settings;
      };
    };
}
