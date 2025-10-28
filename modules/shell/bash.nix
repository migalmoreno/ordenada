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
      apps.shell = "${config.ordenada.features.bash.package}/bin/bash";
    };
  homeManager =
    { config, pkgs, ... }:
    {
      home.sessionVariables = {
        HISTFILE = "${config.ordenada.features.xdg.baseDirs.stateHome}/.bash_history";
      };
      programs.bash = {
        enable = true;
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
