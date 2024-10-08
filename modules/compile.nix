{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.compile = {
      enable = lib.mkEnableOption "compile feature.";
    };
  };
  config = {
    home-manager = mkHomeConfig config "compile" (user: {
      home.packages = with pkgs; [ gnumake ];
      programs.emacs = mkElispConfig {
        name = "ordenada-compile";
        config = ''
          (defun ordenada-compile-ansi-color-apply ()
            "Translate control sequences into text properties in compile buffer."
            (interactive)
            (ansi-color-apply-on-region (point-min) (point-max)))

          (add-hook 'compilation-filter-hook #'ordenada-compile-ansi-color-apply)
        '';
        elispPackages = with pkgs.emacsPackages; [ envrc ];
      };
    });
  };
}
