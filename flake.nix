{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    nix-rice.url = "github:bertof/nix-rice";
    base16.url = "github:SenchoPens/base16.nix";
    arkenfox-nixos.url = "github:dwarfmaster/arkenfox-nixos";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    sops-nix.url = "github:mic92/sops-nix";
  };
  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.modules
        flake-parts.flakeModules.flakeModules
        ./examples
        ./ordenada
      ];
      systems = import inputs.systems;
      flake = {
        flakeModules = {
          ordenada = ./ordenada;
          default = ./ordenada;
        };
      };
      perSystem =
        { pkgs, system, ... }:
        {
          # _module.args.pkgs = import inputs.nixpkgs {
          #   inherit system;
          #   overlays = [
          #     inputs.nur.overlays.default
          #   ];
          # };
          packages = rec {
            docs = pkgs.callPackage ./mkDocs.nix { inherit pkgs; };
            default = docs;
          };
        };
    };
}
