{
  description = "Ordenada is a Reproducible Development Environment for Nix";
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
  };
  outputs =
    inputs@{
      nixpkgs,
      nur,
      nix-rice,
      base16,
      ...
    }:
    let
      ordenada = nixpkgs.lib.callPackagesWith {
        inherit (nixpkgs) lib;
        inherit pkgs;
      } ./lib { };
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        inherit overlays;
      };
      overlay = _final: prev: {
        lib = prev.lib // {
          inherit ordenada;
        };
      };
      overlays = [ overlay ];
    in
    rec {
      lib = ordenada;
      overlays.default = overlay;
      nixosModules.ordenada =
        { pkgs, ... }:
        {
          imports = [ ./modules ];
          config = {
            nixpkgs.overlays = [
              overlays.default
              nur.overlay
              nix-rice.overlays.default
              (final: prev: {
                inputs = inputs;
                lib = prev.lib // {
                  base16 = pkgs.callPackage base16.lib { };
                };
              })
            ];
          };
        };
    };
}
