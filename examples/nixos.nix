{ self, inputs, ... }:

{
  flake.nixosConfigurations.example = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.ordenada
      (
        { config, ... }:
        {
          fileSystems = {
            "/" = {
              label = "NixOS_image";
              fsType = "ext4";
            };
            "/tmp" = {
              fsType = "tmpfs";
            };
          };
          boot.loader.grub = {
            enable = true;
            device = "/dev/sda";
          };
          ordenada.features = {
            networking.enable = true;
            userInfo.enable = true;
            home.enable = true;
            bash.enable = true;
          };
          home-manager.users.${config.ordenada.features.userInfo.username}.imports = [
            self.homeModules.ordenada
            {
              home.stateVersion = config.system.stateVersion;
            }
          ];
          system.stateVersion = "25.05";
        }
      )
    ];
  };
}
