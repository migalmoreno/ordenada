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
      enable = lib.mkEnableOption "the Emacs all-the-icons feature";
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
              (setopt all-the-icons-scale-factor 1.0)
              (setopt all-the-icons-default-adjust 0)
              (setopt all-the-icons-octicon-scale-factor 0.9))
            ${mkIf (hasFeature "emacs.completion" user) ''
              (autoload 'all-the-icons-completion-mode "all-the-icons-completion")
              (all-the-icons-completion-mode)
            ''}

            ${mkIf (hasFeature "emacs.marginalia" user) ''
              (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
            ''}
          '';
          elispPackages =
            with pkgs.emacsPackages;
            [ all-the-icons ] ++ lib.optional (hasFeature "emacs.completion" user) all-the-icons-completion;
        };
      }
    );
  };
}
