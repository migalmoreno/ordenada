{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) types mkOption;
in
{
  options = {
    ordenada.features.keyboard = {
      enable = mkEnableTrueOption "the keyboard feature";
      layout = mkOption {
        description = "The keyboard layout.";
        type = types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              description = "The XKB name of the keyboard layout.";
              default = "us";
            };
            variant = mkOption {
              type = types.str;
              description = "The XKB layout variant name.";
              default = "";
            };
            options = mkOption {
              type = types.listOf types.str;
              description = "The list of XKB options to use for this layout.";
              default = [ ];
            };
          };
        };
        default = {
          name = "us";
          options = [ "ctrl:nocaps" ];
        };
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "keyboard" (user: {
      home.keyboard = with user.features.keyboard; {
        inherit options variant;
        layout = name;
      };
    });
  };
}
