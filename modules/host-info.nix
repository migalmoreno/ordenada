{ lib, mkFeature, ... }:

mkFeature {
  name = "hostInfo";
  options = {
    hostName = lib.mkOption {
      type = lib.types.str;
      description = "The name of the machine.";
      default = "ordenada";
    };
    timeZone = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      description = "The time zone used when displaying times and dates.";
      default = null;
    };
  };
  nixos =
    { config, ... }:
    with config.ordenada.features.hostInfo;
    {
      networking.hostName = hostName;
      time.timeZone = timeZone;
    };
}
