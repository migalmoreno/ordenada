{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.all-the-icons = {
      enable = lib.mkEnableOption "all-the-icons feature.";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.all-the-icons" (
      user: with user.features.emacs; {
        home.packages = with pkgs; [ emacs-all-the-icons-fonts ];
        programs.emacs = mkElispConfig {
          name = "ordenada-all-the-icons";
          config = ''
            (with-eval-after-load 'all-the-icons
              (setq all-the-icons-scale-factor 1.0)
              (setq all-the-icons-default-adjust 0)
              (setq all-the-icons-octicon-scale-factor 0.9))
            ${
              if (hasFeature "emacs.completion" user) then
                ''
                  (autoload 'all-the-icons-completion-mode "all-the-icons-completion")
                  (all-the-icons-completion-mode)
                ''
              else
                ""
            }

            ${
              if (hasFeature "emacs.marginalia" user) then
                ''
                  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
                ''
              else
                ""
            }
          '';
          elispPackages =
            with pkgs.emacsPackages;
            [ all-the-icons ]
            ++ (if (hasFeature "emacs.completion" user) then [ all-the-icons-completion ] else [ ]);
        };
      }
    );
  };
}
