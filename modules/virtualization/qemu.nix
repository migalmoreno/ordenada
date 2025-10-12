{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs.lib.ordenada) mkHomeConfig;
  cfg = config.ordenada.features.qemu;
in
{
  options.ordenada.features.qemu = {
    enable = lib.mkEnableOption "the QEMU feature";
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
