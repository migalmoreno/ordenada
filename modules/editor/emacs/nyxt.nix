{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "nyxt"
  ];
  options = {
    autoStartDelay = lib.mkOption {
      type = lib.types.int;
      description = "Number of seconds to delay the evaluation of Nyxt expressions at startup.";
      default = 0;
    };
    key = lib.mkOption {
      type = lib.types.str;
      description = "Keybinding for nyxt.el map operations.";
      default = "x";
    };
  };
  homeManager =
    { config, pkgs, ... }:
    let
      emacs-nyxt = pkgs.emacsPackages.melpaBuild {
        pname = "nyxt";
        version = "0.2.0";
        src = pkgs.fetchFromGitHub {
          owner = "migalmoreno";
          repo = "nyxt.el";
          rev = "f26834d37ae16e82ac8666a05959182652b412af";
          hash = "sha256-m6xnx4RjKGYej+z5Q1eXuHcQTCeaD4TDHcTSm3oyDuc=";
        };
        packageRequires = with pkgs.emacsPackages; [ sly ];
      };
    in
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-nyxt";
        config =
          with config.ordenada.features; # elisp
          ''
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${emacs.nyxt.key}" 'nyxt-map))

            (with-eval-after-load 'nyxt
              (setopt nyxt-path "${lib.getExe nyxt.package}")
              ${lib.optionalString (nyxt.startupFlags != [ ]) ''
                (setopt nyxt-startup-flags ${ordenada-lib.elisp.toList nyxt.startupFlags})
              ''}
              (setopt nyxt-autostart-delay ${toString emacs.nyxt.autoStartDelay}))
          '';
        elispPackages = [
          emacs-nyxt
        ];
      };
    };
}
