{ lib, mkFeature, ... }:

mkFeature {
  name = "gnupg";
  options = with lib; {
    sshKeys = mkOption {
      type = types.listOf types.str;
      description = "List of SSH key fingerprints.";
      default = [ ];
    };
    pinentryPackage = mkOption {
      type = types.nullOr types.package;
      description = "The package for pinentry input.";
      default = null;
    };
    defaultTtl = mkOption {
      type = types.int;
      description = "The cache TTL for GnuPG operations.";
      default = 86400;
    };
  };
  homeManager =
    { config, ... }:
    {
      services.gpg-agent = with config.ordenada.features.gnupg; {
        inherit sshKeys;
        enable = true;
        defaultCacheTtl = defaultTtl;
        defaultCacheTtlSsh = defaultTtl;
        maxCacheTtl = defaultTtl;
        maxCacheTtlSsh = defaultTtl;
        enableSshSupport = true;
        pinentry.package = pinentryPackage;
      };
      programs.gpg = {
        enable = true;
        homedir = "${config.ordenada.features.xdg.baseDirs.dataHome}/gnupg";
      };
    };
}
