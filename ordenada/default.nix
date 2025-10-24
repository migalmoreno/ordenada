{
  config,
  lib,
  ordenada-lib,
  inputs,
  moduleWithSystem,
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
        nixosModules.ordenada = {
          imports = ordenada-lib.getClassModules "nixos" config.ordenada.modules;
          options.ordenada.globals = globals;
        };
        homeModules.ordenada = moduleWithSystem (
          { system, ... }:
          let
            nur-no-pkgs = import inputs.nur {
              pkgs = null;
              nurpkgs = import inputs.nixpkgs { system = "x86_64-linux"; };
            };
          in
          {
            imports = ordenada-lib.getClassModules "homeManager" config.ordenada.modules ++ [
              nur-no-pkgs.repos.rycee.hmModules.emacs-init
            ];
            options.ordenada.globals = globals;
          }
        );
        darwinModules.ordenada = {
          imports = ordenada-lib.getClassModules "darwin" config.ordenada.modules;
          options.ordenada.globals = globals;
        };
        modules = ordenada-lib.transpose config.ordenada.modules;
      };
    };
}
