{ config, lib, pkgs, ... }:

{
  imports = [
    ./age.nix
    ./gnupg.nix
    ./passage.nix
    ./password-store.nix
    ./ssh.nix
  ];
}
