{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.daemons = {
      enable = lib.mkEnableOption "the Emacs daemons feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.daemons" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-daemons";
        config = ''
          (with-eval-after-load 'ordenada-keymaps
            (define-key ordenada-app-map (kbd "D") #'daemons))
          (add-hook 'daemons-mode-hook #'eldoc-mode)
          (with-eval-after-load 'daemons
            (setopt daemons-list-fill-frame t)
            (setopt daemons-systemd-is-user t))
        '';
        elispPackages = with pkgs.emacsPackages; [ daemons ];
      };
    });
  };
}
