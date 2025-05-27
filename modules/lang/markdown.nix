{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.markdown = {
      enable = lib.mkEnableOption "the Markdown feature";
      headingsScaling = lib.mkEnableOption "incremental scaling of headings";
    };
  };
  config = {
    home-manager = mkHomeConfig config "markdown" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-markdown";
        config = ''
          (with-eval-after-load 'markdown-mode
            ${mkIf user.features.markdown.headingsScaling ''
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
    });
  };
}
