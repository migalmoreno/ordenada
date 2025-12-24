{ lib, mkFeature, ... }:

let
  inherit (lib) mkOption mkIf types;
in
mkFeature {
  name = "gnupg";
  options =
    { config, pkgs, ... }:
    {
      sshKeys = mkOption {
        type = types.listOf types.str;
        description = "List of SSH key fingerprints.";
        default = [ ];
      };
      pinentryPackage = mkOption {
        type = types.nullOr types.package;
        description = "The package for pinentry input.";
        default =
          if (config.ordenada.globals.platform == "darwin") then pkgs.pinentry_mac else pkgs.pinentry-qt;
      };
      defaultTtl = mkOption {
        type = types.int;
        description = "The cache TTL for GnuPG operations.";
        default = 86400;
      };
      storeDir = mkOption {
        type = types.str;
        default = "${config.ordenada.features.xdg.baseDirs.dataHome}/gnupg";
        description = "The directory used for GnuPG.";
      };
      keychainInteraction = mkOption {
        type = types.bool;
        default = true;
        description = "Whether the pinentry should offer an option save the pin in the system keychain (darwin only).";
      };
    };
  ## TODO: [DARWIN] Ttl seems to be ignored, is cached until restart
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
        homedir = config.ordenada.features.gnupg.storeDir;
      };
    };
  darwin =
    { config, ... }:
    {
      launchd.user.agents.pinentry-mac = {
        ## needs access to mac system apps
        path = [
          "/bin"
          "/usr/bin"
          "/usr/local/bin"
        ];
        serviceConfig.RunAtLoad = true;
        script = with config.ordenada.features.gnupg; ''
          export PATH="/bin:/usr/bin:/usr/local/bin:$PATH"
          defaults write org.gpgtools.pinentry-mac UseKeychain -bool ${if keychainInteraction then "YES" else "NO"}
          defaults write org.gpgtools.pinentry-mac DisableKeychain -bool ${if keychainInteraction then "NO" else "YES"}
        '';
      };
    };
}
