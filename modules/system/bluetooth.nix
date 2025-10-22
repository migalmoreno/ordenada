{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = "bluetooth";
  nixos = {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-bluetooth";
        config = ''
          (with-eval-after-load 'ordenada-keymaps
            (keymap-set ordenada-app-map "B" #'bluetooth-list-devices))
          (with-eval-after-load 'bluetooth
            (keymap-set bluetooth-mode-map "C" #'bluetooth-connect-profile)
            (setopt bluetooth-battery-display-warning nil))

          (advice-add 'bluetooth-pa--authorize-service :override #'ignore)
        '';
        elispPackages = with pkgs.emacsPackages; [ bluetooth ];
      };
    };
}
