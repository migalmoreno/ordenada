{ lib, mkFeature, ... }:

mkFeature {
  name = "keyboard";
  options = with lib; {
    layout = mkOption {
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
      description = "The keyboard layout.";
      default = {
        name = "us";
        options = [ "ctrl:nocaps" ];
      };
    };
  };
  darwin =
    { config, ... }:
    with config.ordenada.features.keyboard;
    {
      system.keyboard.enableKeyMapping = true;
      system.keyboard.remapCapsLockToControl = builtins.elem "ctrl:nocaps" layout.options;
    };
  homeManager =
    { config, ... }:
    {
      home.keyboard = with config.ordenada.features.keyboard.layout; {
        inherit options variant;
        layout = name;
      };
    };
  nixos =
    { config, ... }:
    {
      console.keyMap = builtins.elemAt (lib.splitString "," config.ordenada.features.keyboard.layout.name) 0;
    };
}
