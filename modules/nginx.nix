{ config, lib, ... }:

{
  options = {
    ordenada.features.nginx = {
      enable = lib.mkEnableOption "the nginx feature";
    };
  };
  config = lib.mkIf config.ordenada.features.nginx.enable {
    services.nginx = {
      enable = true;
      enableReload = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
    };
    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
  };
}
