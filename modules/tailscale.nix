{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.ordenada.features.tailscale;
in
{
  options = {
    ordenada.features.tailscale = {
      enable = lib.mkEnableOption "the Tailscale feature";
      package = lib.mkOption {
        type = lib.types.package;
        description = "The Tailscale package.";
        default = pkgs.tailscale;
      };
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];
    services.tailscale.enable = true;
    networking.firewall = {
      trustedInterfaces = [ "tailscale0" ];
      allowedUDPPorts = [ config.services.tailscale.port ];
      allowedTCPPorts = [ 22 ];
    };
  };
}
