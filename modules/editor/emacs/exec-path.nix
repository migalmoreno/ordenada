{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "exec-path"
  ];
  options =
    { config, pkgs, ... }:
    {
      envVariables = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of environment variables names to add to emacs.";
        default = [ ];
      };
    };
  homeManager =
    { config, pkgs, ... }:
    let
      listToLines = list: builtins.concatStringsSep "\n" (lib.imap0 (_: var: "\"${var}\"") list);
    in
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-exec-path";
        config =
          with config.ordenada.features.emacs.exec-path; # elisp
          ''
            (require 'exec-path-from-shell)
              (dolist (var
                       '(${
                         if (config.ordenada.globals.platform == "darwin") then
                           ''
                             "EDITOR"
                             "SUDO_ASKPASS"
                             "SSH_AUTH_SOCK"
                             "NIX_SSL_CERT_FILE"
                           ''
                         else
                           ""
                       }
                         ${listToLines envVariables}
                         ))
              (add-to-list 'exec-path-from-shell-variables var))
            (when (memq window-system '(mac ns x))
              (exec-path-from-shell-initialize))
            (when (daemonp)
              (exec-path-from-shell-initialize))
          '';
        elispPackages = with pkgs.emacsPackages; [
          exec-path-from-shell
        ];
      };
    };
}
