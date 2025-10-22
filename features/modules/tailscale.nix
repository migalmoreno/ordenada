{ lib, mkFeature, ... }:

mkFeature {
  name = "tailscale";
  options =
    { pkgs, ... }:
    {
      package = lib.mkOption {
        type = lib.types.package;
        description = "The Tailscale package.";
        default = pkgs.tailscale;
      };
    };
  nixos =
    { config, ... }:
    {
      environment.systemPackages = [ config.ordenada.features.tailscale.package ];
      services.tailscale.enable = true;
      networking.firewall = {
        trustedInterfaces = [ "tailscale0" ];
        allowedUDPPorts = [ config.services.tailscale.port ];
        allowedTCPPorts = [ 22 ];
      };
    };
}
