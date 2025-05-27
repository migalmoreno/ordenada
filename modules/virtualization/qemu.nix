{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.qemu;
in
{
  options = {
    ordenada.features.qemu = {
      enable = lib.mkEnableOption "the QEMU feature";
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      services.spice-vdagentd.enable = true;
      virtualisation.spiceUSBRedirection.enable = true;
    })
    {
      home-manager = mkHomeConfig config "qemu" (user: {
        home.packages = with pkgs; [
          qemu
          quickemu
        ];
      });
    }
  ];
}
