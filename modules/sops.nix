{
  inputs,
  lib,
  mkFeature,
  ...
}:

mkFeature {
  name = "sops";
  options = {
    file = lib.mkOption {
      type = lib.types.path;
      description = "Path to the default SOPS file.";
    };
  };
  nixos =
    { config, ... }:
    {
      imports = [ inputs.sops-nix.nixosModules.sops ];
      sops = with config.ordenada.features; {
        age.sshKeyPaths = age.identities;
        defaultSopsFile = sops.file;
      };
    };
}
