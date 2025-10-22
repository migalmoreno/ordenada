{ mkFeature, ... }:

mkFeature {
  name = "qemu";
  nixos = {
    services.spice-vdagentd.enable = true;
    virtualisation.spiceUSBRedirection.enable = true;
  };
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        qemu
        quickemu
      ];
    };
}
