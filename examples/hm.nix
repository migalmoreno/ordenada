{ self, inputs, ... }:

{
  flake.homeConfigurations.example = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
    modules = [
      self.homeModules.ordenada
      (
        { config, ... }:
        {
          imports = [ ./features.nix ];
          home.stateVersion = "25.05";
        }
      )
    ];
  };
}
