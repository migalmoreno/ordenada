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
      fillFrame = lib.mkEnableOption "showing the list of daemons in the current full frame";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.daemons" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-daemons";
        config = ''
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "D" #'daemons))
          (add-hook 'daemons-mode-hook #'eldoc-mode)
          (with-eval-after-load 'daemons
            (setopt daemons-list-fill-frame ${if user.features.emacs.daemons.fillFrame then "t" else "nil"})
            (setopt daemons-systemd-is-user t))
        '';
        elispPackages = with pkgs.emacsPackages; [ daemons ];
      };
    });
  };
}
