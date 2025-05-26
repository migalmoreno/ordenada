{ config, lib, pkgs, ... }:

with pkgs.lib.ordenada;

let cfg = config.ordenada.features.bash;
in {
  options = {
    ordenada.features.bash = {
      enable = lib.mkEnableOption "the Bash feature";
      package =
        lib.mkPackageOption pkgs "bash" { default = "bashInteractive"; };
    };
  };
  config = lib.mkIf cfg.enable {
    ## TODO: Use a `setGlobal` function here to check for `ordenada.globals.shell === null`
    ##       and print a warning if so
    ordenada.globals.shell = "${cfg.package}/bin/bash";

    home-manager = mkHomeConfig config "bash" (user: {
      home.sessionVariables = {
        HISTFILE = "${user.features.xdg.baseDirs.stateHome}/.bash_history";
      };
      programs.bash = { enable = true; };
      programs.emacs = mkElispConfig {
        name = "ordenada-bash";
        config = ''
          (with-eval-after-load 'shell
            (setopt explicit-shell-file-name "${config.ordenada.globals.shell}"))
        '';
      };
    });
  };
}
