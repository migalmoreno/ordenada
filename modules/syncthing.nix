{ lib, mkFeature, ... }:

mkFeature {
  name = "syncthing";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "syncthing" { };
      devices = lib.mkOption {
        type = lib.types.attrs;
        description = "Set of devices to connect to.";
      };
      folders = lib.mkOption {
        type = lib.types.attrs;
        description = "Set of folders to sync.";
      };
    };
  nixos =
    { config, ... }:
    {
      sops.secrets = with config.ordenada.features; {
        "hosts/${hostInfo.hostName}/syncthing/key".owner = userInfo.username;
        "hosts/${hostInfo.hostName}/syncthing/cert".owner = userInfo.username;
      };
      systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
      services.syncthing = with config.ordenada.features; {
        enable = true;
        package = syncthing.package;
        overrideDevices = true;
        overrideFolders = true;
        extraFlags = [ "--allow-newer-config" ];
        key = config.sops.secrets."hosts/${hostInfo.hostName}/syncthing/key".path;
        cert = config.sops.secrets."hosts/${hostInfo.hostName}/syncthing/cert".path;
        user = userInfo.username;
        dataDir = "${xdg.baseDirs.dataHome}/syncthing";
        configDir = "${xdg.baseDirs.configHome}/syncthing";
        settings = with syncthing; {
          inherit devices folders;
        };
      };
    };
}
