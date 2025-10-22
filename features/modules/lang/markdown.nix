{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "markdown";
  options = {
    headingsScaling = lib.mkEnableOption "incremental scaling of headings";
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-markdown";
        config = ''
          (with-eval-after-load 'markdown-mode
            ${lib.optionalString config.ordenada.features.markdown.headingsScaling ''
              (setopt markdown-header-scaling t)
              (setopt markdown-header-scaling-values '(1.2 1.1 1.1 1.0 1.0 0.9))
            ''}
            (setopt markdown-hide-urls t)
            (setopt markdown-hide-markup t)
            (setopt markdown-command "${pkgs.pandoc}/bin/pandoc")
            (setopt markdown-fontify-code-blocks-natively t))
        '';
        elispPackages = with pkgs.emacsPackages; [ markdown-mode ];
      };
    };
}
