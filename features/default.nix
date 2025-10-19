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
  config = {
    flake = {
      nixosModules.ordenada' = {
        imports = ordenada-lib.getClassModules "nixos" config.ordenada.modules;
      };
      homeModules.ordenada' = {
        imports = ordenada-lib.getClassModules "homeManager" config.ordenada.modules;
      };
      darwinModules.ordenada' = {
        imports = ordenada-lib.getClassModules "darwin" config.ordenada.modules;
      };
      modules = ordenada-lib.transpose config.ordenada.modules;
    };
  };
}
