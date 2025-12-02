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
  homeManager =
    { config, pkgs, ... }:
    let
      listToLines = list: builtins.concatStringsSep "\n" (lib.imap0 (_: var: "\"${var}\"") list) ;
    in
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-exec-path";
        config = # elisp
          ''
            (require 'exec-path-from-shell)
              (dolist (var
              '(
                "SUDO_ASKPASS"
                "NIX_SSL_CERT_FILE"
                ${listToLines config.ordenada.globals.apps.emacs.exec-path}
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
