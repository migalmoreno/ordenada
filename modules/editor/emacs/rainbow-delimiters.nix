{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = [
    "emacs"
    "rainbow-delimiters"
  ];
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-rainbow-delimiters";
        config = ''
          (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
        '';
        elispPackages = with pkgs.emacsPackages; [ rainbow-delimiters ];
      };
    };
}
