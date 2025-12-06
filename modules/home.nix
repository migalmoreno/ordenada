{
  inputs,
  lib,
  mkFeature,
  ...
}:

let
  inherit (lib) mkIf mkOption types;
  commonHmOptions = config: {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.backupFileExtension = "backup";
    home-manager.sharedModules = lib.mkMerge [
      [
        (lib.mkIf config.ordenada.features.home.applyFeaturesToAll {
          ordenada.features = config.ordenada.features;
        })
      ]
    ];
  };
in
mkFeature {
  name = "home";
  options =
    { config, ... }:
    {
      autoStartWmOnTty = mkOption {
        type = types.nullOr types.str;
        description = "The tty to launch the WM in.";
        default = null;
      };
      primaryUser = mkOption {
        default = types.bool;
        example = false;
        description = "Whether this is the primary user of the system.";
        type = types.bool;
      };
      applyFeaturesToAll = mkOption {
        default = config.ordenada.features.home.enable;
        example = true;
        description = "Whether to apply all host features to all Home Manager configurations.";
        type = types.bool;
      };
    };
  darwin =
    { config, ... }:
    with config.ordenada;
    let
      hmVars = config.home-manager.users.${features.userInfo.username}.home.sessionVariables;
      systemPath = config.environment.systemPath;
      fullList = hmVars // {
        EDITOR = config.ordenada.globals.apps.editor;
      };

      setEnvScript = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (name: value: "launchctl setenv ${name} \"${value}\"") fullList
      );
    in
    {
      imports = [ inputs.home-manager.darwinModules.home-manager ];
      home-manager.targets.darwin.linkApps.enable = true; # # TODO: This requires state version 25.11
      system.primaryUser = mkIf features.home.primaryUser features.userInfo.username;

      users.users.${features.userInfo.username}.shell = mkIf (
        globals.apps.shell != null
      ) globals.apps.shell;
      environment.shells = mkIf (globals.apps.shell != null) [
        globals.apps.shell
      ];

      launchd.user.agents.setupEnv = {
        ## needs access to mac system apps
        path = [
          "/bin"
          "/usr/bin"
          "/usr/local/bin"
        ];
        serviceConfig.RunAtLoad = true;
        serviceConfig.UserName = features.userInfo.username;
        script = ''
          ${setEnvScript}
        '';
      };

      ## MacOS doesn't allow nix to change the shell of the user
      ## if the user itself wasn't created by nix (which it most
      ## likely wasn't). We therefore set up a launch daemon that
      ## will force (using `chsh`) the correct shell for the user
      ## every time we switch (`/bin/zsh` is the default shell).
      ## TODO: [DARWIN] Document that this requires a relogin to fully work
      launchd.daemons.defaultShell = {
        ## needs access to mac system apps
        path = [
          "/bin"
          "/usr/bin"
          "/usr/local/bin"
        ];
        serviceConfig.RunAtLoad = true;
        serviceConfig.UserName = "root";
        script = with config.ordenada; ''
          chsh -s ${
            if globals.apps.shell != null then globals.apps.shell else "/bin/zsh"
          } ${features.userInfo.username}
        '';
      };
    }
    // commonHmOptions config;
  nixos =
    { config, ... }:
    {
      imports = [ inputs.home-manager.nixosModules.home-manager ];
      environment.shells =
        with config.ordenada.globals;
        mkIf (apps.shell != null) [
          apps.shell
        ];
      environment.loginShellInit =
        with config.ordenada.features.home;
        (mkIf (autoStartWmOnTty != null) ''
          [[ $(tty) == ${autoStartWmOnTty} ]] && exec ${config.ordenada.globals.apps.wm}
        '');
      i18n.defaultLocale = config.ordenada.features.userInfo.locale;
    }
    // commonHmOptions config;
  homeManager =
    { config, ... }:
    let
      dotProfile = if config.ordenada.globals.platform == "darwin" then ".zprofile" else ".profile";
    in
    {
      programs.home-manager.enable = true;
      targets.genericLinux.enable = mkIf (config.ordenada.globals.platform == "nixos") true;
      home.file.${dotProfile} = mkIf (config.ordenada.globals.apps.shell == null) {
        text = ''
          . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
        '';
      };
    };
}
