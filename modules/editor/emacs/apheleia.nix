{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = [
    "emacs"
    "apheleia"
  ];
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-apheleia";
        config = ''
          (add-hook 'after-init-hook #'apheleia-global-mode)
        '';
        elispPackages = with pkgs.emacsPackages; [ apheleia ];
      };
    };
}
