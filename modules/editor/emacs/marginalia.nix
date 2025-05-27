{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.marginalia = {
      enable = lib.mkEnableOption "the Emacs marginalia feature";
      alignment = lib.mkOption {
        type = lib.types.enum [
          "left"
          "right"
          "center"
        ];
        description = "The alignment of Marginalia annotations.";
        default = "left";
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.marginalia" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-marginalia";
        config = ''
          (add-hook 'after-init-hook #'marginalia-mode)
          (with-eval-after-load 'marginalia
            (setopt marginalia-align '${user.features.emacs.marginalia.alignment}))
        '';
        elispPackages = with pkgs.emacsPackages; [ marginalia ];
      };
    });
  };
}
