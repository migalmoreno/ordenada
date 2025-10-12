{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkOption types;
  inherit (pkgs.lib.ordenada) mkEnableTrueOption mkHomeConfig;
in
{
  options.ordenada.features.ssh = {
    enable = mkEnableOption "the SSH feature";
    daemon = mkEnableTrueOption "the SSH server daemon";
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
  config = lib.mkMerge [
    {
      users = mkHomeConfig config "ssh" (user: {
        openssh.authorizedKeys.keys = user.features.ssh.authorizedKeys;
      });
      home-manager = mkHomeConfig config "ssh" (user: {
        services.ssh-agent.enable =
          !config.home-manager.users.${user.name}.services.gpg-agent.enableSshSupport;
        programs.ssh = {
          enable = true;
          matchBlocks = user.features.ssh.matchBlocks;
        };
      });
    }
  ];
}
