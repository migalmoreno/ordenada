{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "marginalia"
  ];
  options.alignment = lib.mkOption {
    type = lib.types.enum [
      "left"
      "right"
      "center"
    ];
    description = "The alignment of Marginalia annotations.";
    default = "left";
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-marginalia";
        config = ''
          (add-hook 'after-init-hook #'marginalia-mode)
          (with-eval-after-load 'marginalia
            (setopt marginalia-align '${config.ordenada.features.emacs.marginalia.alignment}))
        '';
        elispPackages = with pkgs.emacsPackages; [ marginalia ];
      };
    };
}
