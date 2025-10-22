{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "ssh";
  options = with lib; {
    daemon = ordenada-lib.mkEnableTrueOption "the SSH server daemon";
    matchBlocks = mkOption {
      type = types.attrs;
      description = "The SSH stanzas to use in the client configuration.";
      default = { };
    };
    authorizedKeys = mkOption {
      type = types.listOf types.str;
      description = "List of user authorized SSH keys.";
      default = [ ];
    };
  };
  homeManager =
    { config, ... }:
    {
      services.ssh-agent.enable = !config.services.gpg-agent.enableSshSupport;
      programs.ssh = {
        enable = true;
        matchBlocks = config.ordenada.features.ssh.matchBlocks;
      };
    };
}
