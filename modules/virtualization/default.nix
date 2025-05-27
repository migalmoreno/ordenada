{ config, lib, pkgs, ... }:

{
  imports = [
    ./docker.nix
    ./qemu.nix
  ];
}
