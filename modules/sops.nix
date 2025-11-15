{
  inputs,
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "sops";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "sops" { };
      file = lib.mkOption {
        type = lib.types.path;
        description = "Path to the default SOPS file.";
      };
    };
  nixos =
    { config, ... }:
    {
      imports = [ inputs.sops-nix.nixosModules.sops ];
      sops = with config.ordenada.features; {
        age.sshKeyPaths = age.identities;
        defaultSopsFile = sops.file;
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-sops";
        config = # elisp
          ''
            (global-sops-mode 1)
            (defvar-keymap ordenada-sops-mode-map
              "C-c C-c" #'sops-save-file
              "C-c C-k" #'sops-cancel
              "C-c C-d" #'sops-edit-file)

            (define-minor-mode ordenada-sops-mode
              "Set up local keybindings for `sops-mode'."
              :keymap ordenada-sops-mode-map)

            (add-hook 'sops-mode-hook #'ordenada-sops-mode)

            (with-eval-after-load 'sops
              (setopt sops-executable "${lib.getExe config.ordenada.features.sops.package}"))
          '';
        elispPackages = with pkgs.emacsPackages; [ sops ];
      };
    };
}
