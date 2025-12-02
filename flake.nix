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
    nixos-wsl.url = "github:nix-community/nixos-wsl/main";
    sops-nix.url = "github:mic92/sops-nix";
    nx-router.url = "github:migalmoreno/nx-router";
    nx-tailor.url = "github:migalmoreno/nx-tailor";
    nx-mosaic.url = "github:migalmoreno/nx-mosaic";
    nx-search-engines = {
      url = "github:aartaka/nx-search-engines";
      flake = false;
    };
    emacs-darwin = {
      url = "github:nix-giant/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
        { pkgs, ... }:
        {
          formatter = pkgs.nixfmt-rfc-style;
          packages = rec {
            docs = pkgs.callPackage ./mkDocs.nix { inherit pkgs; };
            default = docs;
          };
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              home-manager
              nixos-rebuild
            ];
          };
        };
    };
}
