{ self, inputs, ... }:

{
  flake.darwinConfigurations.example = inputs.darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    modules = [
      self.darwinModules.ordenada
      inputs.home-manager.darwinModules.home-manager
      (
        { config, ... }:
        {
          imports = [
            ./features.nix
            ./features-darwin.nix
          ];
          home-manager.users.${config.ordenada.features.userInfo.username}.imports = [
            self.homeModules.ordenada
            {
              home.stateVersion = "25.05";
            }
          ];
          system.stateVersion = 6;
        }
      )
    ];
  };
}
