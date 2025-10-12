{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
    nur.url = "github:nix-community/NUR";
    nix-rice.url = "github:bertof/nix-rice";
    base16.url = "github:SenchoPens/base16.nix";
    arkenfox-nixos.url = "github:dwarfmaster/arkenfox-nixos";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    inputs@{
      nixpkgs,
      nur,
      nix-rice,
      base16,
      home-manager,
      flake-parts,
      ...
    }:
    let
      ordenada = nixpkgs.lib.callPackagesWith {
        inherit (nixpkgs) lib;
        pkgs = pkgs';
      } ./lib { };
      pkgs' = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ overlay ];
      };
      overlay = _final: prev: {
        lib = prev.lib // {
          inherit ordenada;
          base16 = pkgs'.callPackage base16.lib { };
        };
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      flake = {
        overlays.default = overlay;
        lib = ordenada;
        nixosModules.ordenada =
          { pkgs, ... }:
          {
            imports = [
              ./modules
              home-manager.nixosModules.home-manager
            ];
            config = {
              nixpkgs.overlays = [
                overlay
                nur.overlays.default
                nix-rice.overlays.default
                (final: prev: { inherit inputs; })
              ];
            };
          };
      };
      perSystem =
        { pkgs, ... }:
        {
          packages = rec {
            docs = pkgs.callPackage ./mkDocs.nix {
              pkgs = pkgs';
            };
            default = docs;
          };
        };
    };
}
