{ lib, ... }:

let
  inherit (lib) types mkOption;
in
{
  options = {
    ordenada.features.keyboard = {
      layout = mkOption {
        description = "The keyboard layout.";
        type = types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              description = "The XKB name of the keyboard layout.";
              default = "en";
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
          name = "en";
          options = [ "ctrl:nocaps" ];
        };
      };
    };
  };
}
