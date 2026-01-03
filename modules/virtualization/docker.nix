{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  inherit (lib)
    mkOption
    mkMerge
    mkIf
    types
    ;
in
mkFeature {
  name = "docker";
  options = {
    key = mkOption {
      type = types.str;
      default = "D";
      description = "Keybinding to launch Emacs Docker interface.";
    };
    colimaFlags = mkOption {
      type = types.attrs;
      default = { };
      example = {
        memory = 4;
        cpu = 2;
      };
      description = "Configuration for the colima runtime (darwin only).";
    };
  };
  nixos = {
    virtualisation.docker.enable = true;
    ordenada.features.userInfo.extraGroups = [ "docker" ];
  };
  homeManager =
    { config, pkgs, ... }:
    with config.ordenada.features;
    mkMerge [
      (mkIf (config.ordenada.globals.platform == "darwin") {
        home.packages = with pkgs; [
          colima
          lima
        ];
        home.sessionVariables = {
          COLIMA_HOME = "${xdg.baseDirs.stateHome}/colima";
        };
        launchd.agents.start-colima =
          let
            label = "start-colima";
          in
          {
            enable = true;
            config = with userInfo; {
              Label = "org.ordenada.${label}";
              RunAtLoad = true;
              StandardErrorPath = "${homeDirectory}/Library/Logs/ordenada/${label}/error.log";
              StandardOutPath = "${homeDirectory}/Library/Logs/ordenada/${label}/output.log";
              EnvironmentVariables = {
                COLIMA_HOME = "${xdg.baseDirs.stateHome}/colima";
              };
              ProgramArguments =
                let
                  startColima = pkgs.writeShellScript label ''
                      export PATH="${
                        lib.makeBinPath [
                          "/bin"
                          "/usr/bin"
                          "/usr/local/bin"
                          pkgs.docker
                        ]
                      }"
                    ${pkgs.colima}/bin/colima start ${ordenada-lib.attrsToFlags { separator = " "; } docker.colimaFlags}
                    ${pkgs.docker}/bin/docker context use colima
                  '';
                in
                [ "${startColima}" ];
            };
          };
      })
      {
        home.packages = with pkgs; [ docker ];
        programs.emacs = ordenada-lib.mkElispConfig pkgs {
          name = "ordenada-docker";
          config = # elisp
            ''
              (with-eval-after-load 'ordenada-keymaps
                (keymap-set ordenada-app-map "${docker.key}" #'docker))
              (add-to-list 'auto-mode-alist '(".*Dockerfile\\'" . dockerfile-mode))
            '';
          elispPackages = with pkgs.emacsPackages; [
            docker
            dockerfile-mode
          ];
        };
      }
    ];
}
