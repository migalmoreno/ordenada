{ config, lib, pkgs, ... }:

{
  imports = [
    ./android.nix
    ./compile.nix
    ./direnv.nix
  ];
}
