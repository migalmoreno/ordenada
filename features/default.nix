{
  config,
  lib,
  ordenada-lib,
  ...
}:

let
  inherit (lib) mkOption types;
  autoload = import ./autoload.nix { inherit lib; };
in
{
  imports = [
    ./lib.nix
  ] ++ autoload;
  options.ordenada = {
    modules = mkOption {
      type = types.lazyAttrsOf (types.lazyAttrsOf types.deferredModule);
    };
  };
  config =
    let
      globals = {
        shell = mkOption {
          type = types.nullOr types.str;
          description = "The system wide used shell.";
          default = null;
        };
        wayland = mkOption {
          type = types.nullOr types.bool;
          description = "Whether or not the WM is running under wayland.";
          default = false;
        };
        wm = mkOption {
          type = types.nullOr types.str;
          description = "The system wide used window manager.";
          default = null;
        };
        launcher = mkOption {
          type = types.nullOr types.str;
          description = "The system wide used application launcher.";
          default = null;
        };
        bar = mkOption {
          type = types.nullOr types.str;
          description = "The system wide used bar.";
          default = null;
        };
      };
    in
    {
      flake = {
        nixosModules.ordenada' = {
          imports = ordenada-lib.getClassModules "nixos" config.ordenada.modules;
          options.ordenada = {
            inherit globals;
          };
        };
        homeModules.ordenada' = {
          imports = ordenada-lib.getClassModules "homeManager" config.ordenada.modules;
          options.ordenada = {
            inherit globals;
          };
        };
        darwinModules.ordenada' = {
          imports = ordenada-lib.getClassModules "darwin" config.ordenada.modules;
          options.ordenada = {
            inherit globals;
          };
        };
        modules = ordenada-lib.transpose config.ordenada.modules;
      };
    };
}
