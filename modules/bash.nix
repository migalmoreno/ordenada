{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.bash = {
      enable = lib.mkEnableOption "the Bash feature";
      package = lib.mkPackageOption pkgs "bash" { default = "bashInteractive"; };
    };
  };
  config = {
    home-manager = mkHomeConfig config "bash" (user: {
      home.sessionVariables = {
        HISTFILE = "${user.features.xdg.baseDirs.stateHome}/.bash_history";
      };
      programs.bash = {
        enable = true;
      };
      programs.emacs.extraConfig = ''
        (with-eval-after-load 'shell
          (setq explicit-shell-file-name "${user.features.bash.package}/bin/bash"))
      '';
    });
  };
}
