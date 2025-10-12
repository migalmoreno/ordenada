{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkPackageOption
    mkOption
    types
    ;
  inherit (pkgs.lib.ordenada) mkHomeConfig;
  cfg = config.ordenada;
  accountModule = types.submodule {
    options = {
      primary = mkEnableOption "this account as the primary mail account";
      flavor = mkOption {
        description = "A pre-defined mail account flavor.";
        type = types.str;
        default = "plain";
      };
      fqda = mkOption {
        description = "Email address of the mail account.";
        type = types.str;
        default = cfg.features.userInfo.email;
      };
      fullName = mkOption {
        description = "Full name of the mail account.";
        type = types.str;
        default = cfg.features.userInfo.fullName;
      };
      signature = mkOption {
        description = "The email signature for the mail account.";
        type = types.str;
        default = cfg.features.mail.defaultMessageSignature;
      };
      extraConfig = mkOption {
        description = "Extra config attrset to pass to home-manager's email account.";
        type = types.attrs;
        default = { };
      };
    };
  };
in
{
  options.ordenada.features.mail = {
    enable = mkEnableOption "the mail feature";
    accounts = mkOption {
      type = types.attrsOf accountModule;
      description = "The list of mail accounts.";
      default = { };
    };
    mbsync = {
      enable = mkOption {
        type = types.bool;
        default = config.ordenada.features.mail.enable;
        description = "Whether to enable the mbsync mail feature.";
        example = true;
      };
      package = mkPackageOption pkgs "isync" { };
    };
    msmtp = {
      enable = mkOption {
        type = types.bool;
        default = config.ordenada.features.mail.enable;
        description = "Whether to enable the msmtp mail feature.";
        example = true;
      };
      package = mkPackageOption pkgs "msmtp" { };
    };
    imapnotify = {
      enable = mkOption {
        type = types.bool;
        default = config.ordenada.features.mail.enable;
        description = "Whether to enable the imapnotify mail feature.";
        example = true;
      };
      package = mkPackageOption pkgs "goimapnotify" { };
    };
    defaultMessageSignature = mkOption {
      description = "The default email signature.";
      type = types.str;
      default = ''
        Best regards,
        ${cfg.features.userInfo.fullName}
      '';
    };
  };
  config.home-manager = mkHomeConfig config "mail" (user: {
    programs = {
      inherit (user.features.mail) mbsync msmtp;
    };
    services.imapnotify.enable = user.features.mail.imapnotify.enable;
    accounts.email = {
      maildirBasePath = "${user.features.xdg.baseDirs.stateHome}/mail";
      accounts = lib.mapAttrs (
        name: acc:
        lib.recursiveUpdate {
          primary = acc.primary;
          maildir.path = "accounts/${acc.fqda}";
          signature = {
            showSignature = "append";
            text = acc.signature;
          };
          address = acc.fqda;
          realName = acc.fullName;
          passwordCommand =
            with config.home-manager.users.${user.name}.programs;
            (toString (
              pkgs.writeShellScript "imapnotify-pass" ''
                export GNUPGHOME=${gpg.homedir}
                export PASSWORD_STORE_DIR=${password-store.settings.PASSWORD_STORE_DIR}
                ${user.features.password-store.package}/bin/pass show mail/${acc.fqda}
                ${gpg.package}/bin/gpg-connect-agent updatestartuptty /bye > /dev/null
              ''
            ));
          gpg = lib.mkIf (user.features.userInfo.gpgPrimaryKey != null) {
            key = user.features.userInfo.gpgPrimaryKey;
            signByDefault = true;
          };
          mbsync = lib.mkIf user.features.mail.mbsync.enable {
            enable = true;
            create = "maildir";
          };
          msmtp.enable = user.features.mail.msmtp.enable;
          imapnotify = lib.mkIf user.features.mail.imapnotify.enable {
            enable = true;
            boxes = [ "Inbox" ];
            onNotify = "${user.features.mail.mbsync.package}/bin/mbsync ${name}";
            onNotifyPost = "${pkgs.libnotify}/bin/notify-send 'New mail received'";
          };
          userName = acc.fqda;
          flavor = acc.flavor;
        } acc.extraConfig
      ) user.features.mail.accounts;
    };
  });
}
