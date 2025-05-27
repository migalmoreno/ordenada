{ config, lib, pkgs, ... }:

{
  imports = [
    ./bluetooth.nix
    ./fontutils.nix
    ./keyboard.nix
    ./networking.nix
    ./pipewire.nix
  ];
}
