{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.gnupg = {
      enable = lib.mkEnableOption "the GnuPG feature";
      sshKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of SSH key fingerprints.";
        default = [ ];
      };
      pinentryPackage = lib.mkOption {
        type = lib.types.package;
        description = "The package for pinentry input.";
        default = pkgs.pinentry-bemenu;
      };
      defaultTtl = lib.mkOption {
        type = lib.types.int;
        description = "The cache TTL for GnuPG operations.";
        default = 86400;
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "gnupg" (user: {
      services.gpg-agent = with user.features.gnupg; {
        enable = true;
        defaultCacheTtl = defaultTtl;
        enableSshSupport = true;
        pinentryPackage = pinentryPackage;
        sshKeys = sshKeys;
      };
      programs = {
        gpg = {
          enable = true;
          homedir = "/home/${user.features.userInfo.username}/.local/share/gnupg";
        };
      };
    });
  };
}
