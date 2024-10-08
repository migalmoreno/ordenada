{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.docker;
in
{
  options = {
    ordenada.features.docker = {
      enable = lib.mkEnableOption "the Docker feature";
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable { virtualisation.docker.enable = true; })
    {
      users = mkHomeConfig config "docker" (user: {
        extraGroups = [ "docker" ];
      });
    }
  ];
}
