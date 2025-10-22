{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = "direnv";
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ direnv ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-direnv";
        config = ''
          (eval-when-compile (require 'envrc))
          (add-hook 'after-init-hook #'envrc-global-mode)
          (with-eval-after-load 'envrc
            (keymap-set envrc-mode-map "C-c E" #'envrc-command-map))
        '';
        elispPackages = with pkgs.emacsPackages; [ envrc ];
      };
    };
}
