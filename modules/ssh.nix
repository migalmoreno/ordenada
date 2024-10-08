{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.ssh;
in
{
  options = {
    ordenada.features.ssh = {
      enable = lib.mkEnableOption "the SSH feature";
      daemon = mkEnableTrueOption "the SSH server daemon";
      matchBlocks = lib.mkOption {
        type = lib.types.attrs;
        description = "The SSH stanzas to use in the client configuration.";
        default = { };
      };
      rootAuthorizedKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of root authorized SSH keys.";
        default = [ ];
      };
      userAuthorizedKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of user authorized SSH keys.";
        default = cfg.rootAuthorizedKeys;
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf (cfg.enable && cfg.daemon) {
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          KbdInteractiveAuthentication = false;
          PermitRootLogin = "yes";
        };
      };
      networking.firewall.allowedTCPPorts = [ 22 ];
      users.users.root = {
        openssh.authorizedKeys.keys = cfg.rootAuthorizedKeys;
      };
    })
    {
      users = mkHomeConfig config "ssh" (user: {
        openssh.authorizedKeys.keys = user.features.ssh.userAuthorizedKeys;
      });
      home-manager = mkHomeConfig config "ssh" (user: {
        programs.ssh = {
          enable = true;
          matchBlocks = user.features.ssh.matchBlocks;
        };
      });
    }
  ];
}
