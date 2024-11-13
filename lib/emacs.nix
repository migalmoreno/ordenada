{ lib, pkgs, ... }:

{
  mkElispConfig =
    {
      name,
      config,
      elispPackages ? [ ],
      summary ? "No description provided",
      earlyInit ? false,
      addToInitEl ? true,
    }:
    let
      pkg = pkgs.emacsPackages.trivialBuild {
        pname = name;
        version = "0.1.0";
        src = pkgs.writeText "${name}.el" ''
          ;;; ${name}.el --- ${summary} -*- lexical-binding: t -*-

          ${config}
          (provide '${name})'';
        propagatedBuildInputs = elispPackages;
      };
    in
    {
      extraPackages = epkgs: elispPackages ++ [ pkg ];
      extraConfig = ''
        ${if addToInitEl then "(require '${pkg.pname})" else ""} 
      '';
    }
    // (lib.optionalAttrs earlyInit {
      init = {
        enable = true;
        earlyInit = earlyInit;
      };
    });
}
