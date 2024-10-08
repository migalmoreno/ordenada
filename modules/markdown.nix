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
          (with-eval-after-load 'markdown
            ${
              if user.features.markdown.headingsScaling then
                ''
                  (setq markdown-header-scaling t)
                  (setq markdown-header-scaling-values '(1.2 1.1 1.1 1.0 1.0 0.9))
                ''
              else
                ""
            }
            (setq markdown-hide-urls t)
            (setq markdown-hide-markup t)
            (setq markdown-command "${pkgs.pandoc}/bin/pandoc")
            (setq markdown-fontify-code-blocks-natively t))
        '';
        elispPackages = with pkgs.emacsPackages; [ markdown-mode ];
      };
    });
  };
}
