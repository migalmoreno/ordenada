{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "bash";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "bash" { default = "bashInteractive"; };
    };
  globals =
    { config, ... }:
    {
      apps.shell = lib.mkIf config.ordenada.features.bash.enable "${config.ordenada.features.bash.package}/bin/bash";
    };
  darwin =
    { config, ... }:
    {
      programs.bash.enable = true;
    };
  homeManager =
    { config, pkgs, ... }:
    {
      home.sessionVariables = {
        HISTFILE = "${config.ordenada.features.xdg.baseDirs.stateHome}/.bash_history";
      };
      programs.bash = with config.ordenada.features.bash; {
        enable = true;
        package = package;
      };
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-bash";
        config = ''
          (with-eval-after-load 'shell
            (setopt explicit-shell-file-name "${config.ordenada.globals.apps.shell}"))
        '';
      };
    };
}
