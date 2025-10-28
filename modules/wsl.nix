{
  lib,
  inputs,
  mkFeature,
  ...
}:

mkFeature {
  name = "wsl";
  nixos =
    { config, pkgs, ... }:
    {
      imports = [ inputs.nixos-wsl.nixosModules.default ];
      wsl = with config.ordenada.features.userInfo; {
        enable = true;
        defaultUser = username;
        interop.register = true;
        wslConf = {
          network.generateResolvConf = true;
          network.generateHosts = true;
          user.default = username;
        };
      };
      systemd.services.wsl-vpnkit = {
        enable = true;
        description = "wsl-vpnkit";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          ExecStart = "${pkgs.wsl-vpnkit}/bin/wsl-vpnkit";
          Restart = "on-failure";
          KillMode = "mixed";
        };
      };
    };
  homeManager =
    { config, pkgs, ... }:
    lib.mkIf config.ordenada.features.sway.enable {
      systemd.user.services.wsl-clipboard = {
        Unit = {
          Description = "WSL clipboard sharing";
          After = [ "sway-session.target" ];
        };
        Install = {
          WantedBy = [ "default.target" ];
        };
        Service = {
          Type = "exec";
          ExecStart = "${pkgs.wl-clipboard}/bin/wl-paste --watch /mnt/c/Windows/System32/clip.exe";
          Restart = "on-failure";
        };
      };
    };
}
